using System;
using System.Collections.Generic;
using System.ServiceModel;
using System.ServiceModel.Web;
using System.ServiceModel.Description;
using Newtonsoft.Json;
using FunWAPLib;

namespace CWebService
{
    [ServiceContract]
    public interface IDSyncService
    {

        [OperationContract]
        [WebInvoke(Method = "POST", UriTemplate = "DAsync")]
        string DistributeAsync(string json);

    }

    public class DSyncService : IDSyncService
    {
        public string DistributeAsync(string json)
        {
            Console.WriteLine("end deserialization");
            DeserializeDasyncRequest deserializer = new DeserializeDasyncRequest();
            Dictionary<string, object> funCall = deserializer.DeserializeRequest(json);
            string ret = FunWap.InterpretFunction(funCall);
            string jsonRes = JsonConvert.SerializeObject(ret);
            return jsonRes;
        }

    }

    class Program
    {
        static void Main(string[] args)
        {
            Uri baseAddress = new Uri("http://localhost:8080/RestService");

            // Create the ServiceHost.
            using (ServiceHost host = new ServiceHost(typeof(DSyncService), baseAddress))
            {
                // Enable metadata publishing.
                ServiceMetadataBehavior smb = new ServiceMetadataBehavior();
                smb.HttpGetEnabled = true;
                smb.MetadataExporter.PolicyVersion = PolicyVersion.Policy15;
                host.Description.Behaviors.Add(smb);

                // Open the ServiceHost to start listening for messages. Since
                // no endpoints are explicitly configured, the runtime will create
                // one endpoint per base address for each service contract implemented
                // by the service.
                host.Open();

                Console.WriteLine("The service is ready at {0}", baseAddress);
                Console.WriteLine("Press <Enter> to stop the service.");
                Console.ReadLine();

                // Close the ServiceHost.
                host.Close();
            }       
        }
    }
}
