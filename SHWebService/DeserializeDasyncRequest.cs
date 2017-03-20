using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Newtonsoft.Json;
using AP;

namespace CWebService
{
    class DeserializeDasyncRequest
    {
        // Function DeserializeRequest: take a Json request as input and deserialize it in a Dictionary
        public Dictionary<string, object> DeserializeRequest(string request)
        {
            int codeStartIndex = request.IndexOf("\"code\":");
            int nameFunIndex = request.IndexOf("\"nameFun\":");
            int paramIndex = request.IndexOf("\"param\":");
            string codeDef = request.Substring(codeStartIndex + 8, nameFunIndex - codeStartIndex - 10);
            string nameFun = request.Substring(nameFunIndex + 11, paramIndex - codeDef.Length - 25);
            int parDxIndex = request.LastIndexOf("]");
            int parSxIndex = request.IndexOf("[");
            string paramListAsString = request.Substring(paramIndex + 8, parDxIndex - parSxIndex + 1);
            string[] paramList = JsonConvert.DeserializeObject<string[]>(paramListAsString);
            List<AP.eval> evalParam = this.DeserializeParamList(paramList);
            Dictionary<string, object> res = new Dictionary<string, object>();
            res.Add("code", codeDef);
            res.Add("nameFun", nameFun);
            res.Add("params", evalParam);
            return res;
        }

        public List<AP.eval> DeserializeParamList(string[] paramList)
        {
            List<AP.eval> res = new List<AP.eval>();
            for (int i = 0; i < paramList.Length; i++){
                if (paramList[i].Contains("Int"))
                {
                    var aux = paramList[i].Replace("Int","");
                    res.Add(AP.eval.NewInt((int)Convert.ToInt64(aux)));

                }
                else if (paramList[i].Contains("Bool"))
                {
                    // Parse bool eval
                    if(paramList[i].Contains("true"))
                        res.Add(AP.eval.NewBool(true));
                    else
                        res.Add(AP.eval.NewBool(false));
                }
                else if (paramList[i].Contains("Funval"))
                {
                    // Parse funval eval
                }

            }
            return res;
        }
    }
}
