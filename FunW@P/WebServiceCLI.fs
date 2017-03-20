namespace AP

open System
open System.Xml
open System.Collections.Generic
open System.Runtime.Serialization
open System.Runtime.Serialization.Json
open System.IO
open System.Text
open System.Net
open Microsoft.FSharp.Reflection
open System.Web.Script.Serialization
open System.Net.Http
open Newtonsoft.Json
open AP.Ast

// WebService client for the asynchronous operation via WebService
module WebServiceCLI =
   
    let DAsyncPOST (baseAddress :string) (code:string) (nameFun :string) (elist :eval list) =
        // Prepare the request for remote web service
        let myreq = System.Net.WebRequest.CreateHttp(baseAddress + "DAsync")
        myreq.Method <- "POST"
        myreq.ContentType <- "application/json"
        myreq.KeepAlive <- false
        
        let map : Dictionary<string, obj> = new Dictionary<string,obj>()

        // Prepare the object to send with code, name of the function and parameters
        let s = elist |> List.map(fun item -> sprintf "%A" item)
        map.Add("code", code)
        map.Add("nameFun", nameFun)
        map.Add("param", s)

        let json = JavaScriptSerializer().Serialize(map)
        let s = sprintf "%A" json
        let json2 = JavaScriptSerializer().Serialize(s)
        let byteJson = System.Text.Encoding.Default.GetBytes(json2)
        myreq.ContentLength <- int64(byteJson.Length)
        let postStream = myreq.GetRequestStream()
        postStream.Write(byteJson,0, byteJson.Length)
        postStream.Close()
        // Wait the response from the remote web service
        let mutable stringResult : string = ""
        try
            let postResponse = myreq.GetResponse()
            let encoding = System.Text.Encoding.UTF8
            let stream = new StreamReader(postResponse.GetResponseStream(), encoding)
            stringResult <- stream.ReadToEnd()
            stream.Close()
        with | :? System.Net.WebException as ex -> 
            let postResponse = ex.Response
            let encoding = System.Text.Encoding.UTF8
            let stream = new StreamReader(postResponse.GetResponseStream(), encoding)
            let stringResult = stream.ReadToEnd() 
            stream.Close()
            failwith stringResult
        stringResult