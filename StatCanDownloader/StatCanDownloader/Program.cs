namespace StatCanDownloader
{
    using CommandLine;
    using CsvHelper;
    using Newtonsoft.Json;
    using StatCanDownloader.Utilities;
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    using System.Net;
    using static StatCanDownloader.Program.ResponseDto;

    class Program
    {
        public class Options
        {
            [Option('f', "file", Required = true, HelpText = "The file from which we read our StatCan vector id codes.")]
            public string IdFile { get; set; }
        }

        static void Main(string[] args)
        {
            Parser.Default.ParseArguments<Options>(args)
                .WithParsed(o =>
                {
                    var vectorIds = ReadIdsFromFile(o.IdFile).Distinct();
                    var vectorChunks = vectorIds.GetBlock(300);
                    var startDate = "1980-1-01T08:30";
                    var endDate = DateTime.Now.ToString("yyyy-M-ddTHH:mm");

                    try
                    {
                        using (var fd = File.Create("output.csv"))
                        using (var streamWriter = new StreamWriter(fd))
                        using (var csvWriter = new CsvHelper.CsvWriter(streamWriter))
                        {
                            csvWriter.WriteField("ResponseStatusCode");
                            csvWriter.WriteField("ProductId");
                            csvWriter.WriteField("Coordinate");
                            csvWriter.WriteField("VectorId");

                            csvWriter.WriteField("RefPer");
                            csvWriter.WriteField("RefPer2");
                            csvWriter.WriteField("RefPerRaw");
                            csvWriter.WriteField("RefPerRaw2");
                            csvWriter.WriteField("Value");
                            csvWriter.WriteField("Decimals");
                            csvWriter.WriteField("ScalarFactorCode");
                            csvWriter.WriteField("SymbolCode");
                            csvWriter.WriteField("StatusCode");
                            csvWriter.WriteField("SecurityLevelCode");
                            csvWriter.WriteField("ReleaseTime");
                            csvWriter.WriteField("FrequencyCode");

                            csvWriter.NextRecord();

                            DownloadData(vectorChunks, startDate, endDate, csvWriter);
                        }
                    }
                    catch (IOException)
                    {
                        Console.WriteLine("Can't write to output file. Make sure it's not open in Excel.");
                    }
                });

            Console.WriteLine("Press any key to continue...");
            Console.ReadKey();
        }

        private static IEnumerable<string> ReadIdsFromFile(string filename)
        {
            using (var fd = File.OpenRead(filename))
            using (var reader = new StreamReader(fd))
            {
                while (!reader.EndOfStream)
                {
                    var newLine = reader.ReadLine().Trim();
                    int blah = 0;
                    if (!string.IsNullOrEmpty(newLine) && int.TryParse(newLine, out blah))
                        yield return newLine;
                }
            }
        }

        private static IEnumerable<string> DownloadData(
            IEnumerable<IEnumerable<string>> vectorIdBlocks,
            string startDate,
            string endDate,
            CsvWriter csvWriter)
        {
            var productIds = new HashSet<string>();
            foreach (var block in vectorIdBlocks)
            {
                DownloadData(block, startDate, endDate, csvWriter).ToArray();
                Console.WriteLine("Downloading new block of vector ids...");
            }

            return productIds;
        }

        private static IEnumerable<string> DownloadData(IEnumerable<string> vectorIds, string startDate, string endDate, CsvWriter csvWriter)
        {
            var payload = new PayloadDto 
            {
                vectorIds = vectorIds,
                startDataPointReleaseDate = startDate,
                endDataPointReleaseDate = endDate,
            };

            var request = (HttpWebRequest)WebRequest.Create(Constants.StatCanUrl);
            request.ContentType = "application/json";
            request.Accept = "application/json";
            request.Method = "POST";

            using (var writer = new StreamWriter(request.GetRequestStream()))
            {
                writer.Write(Newtonsoft.Json.JsonConvert.SerializeObject(payload));
            }

            var rep = (HttpWebResponse)request.GetResponse();

            using (var reader = new StreamReader(rep.GetResponseStream()))
            {
                var records = Newtonsoft.Json.JsonConvert.DeserializeObject<IEnumerable<ResponseDto>>(reader.ReadToEnd(), new JsonSerializerSettings() { NullValueHandling = NullValueHandling.Ignore });

                if (records.Any())
                {
                    var vectors = records.Select(r => r.Vector);
                    foreach (var v in vectors)
                    {
                        foreach (var s in v.VectorDataPoint)
                        {
                            csvWriter.WriteField(v.ResponseStatusCode);
                            csvWriter.WriteField(v.ProductId);
                            csvWriter.WriteField(v.Coordinate);
                            csvWriter.WriteField(v.VectorId);

                            csvWriter.WriteField(s.RefPer);
                            csvWriter.WriteField(s.RefPer2);
                            csvWriter.WriteField(s.RefPerRaw);
                            csvWriter.WriteField(s.RefPerRaw2);
                            csvWriter.WriteField(s.Value);
                            csvWriter.WriteField(s.Decimals);
                            csvWriter.WriteField(s.ScalarFactorCode);
                            csvWriter.WriteField(s.SymbolCode);
                            csvWriter.WriteField(s.StatusCode);
                            csvWriter.WriteField(s.SecurityLevelCode);
                            csvWriter.WriteField(s.ReleaseTime);
                            csvWriter.WriteField(s.FrequencyCode);

                            csvWriter.NextRecord();
                        }

                        yield return v.ProductId;
                    }
                }
                else
                {
                    Console.WriteLine("Empty response recorded...");
                }
            }
        }

        private static class Constants
        {
            public static readonly string StatCanUrl = "https://www150.statcan.gc.ca/t1/wds/rest/getBulkVectorDataByRange";
            public static readonly string MetadataUrl = "https://www150.statcan.gc.ca/t1/wds/rest/getCubeMetadata";
        }

        private class PayloadDto
        {
            public IEnumerable<string> vectorIds { get; set; }
            public string startDataPointReleaseDate { get; set; }
            public string endDataPointReleaseDate { get; set; }
        }

        public class ResponseDto
        {
            public string Status { get; set; }

            [JsonProperty("object")]
            public VectorDto Vector {get;set;} 

            public class VectorDto
            {
                public int ResponseStatusCode { get; set; }
                public string ProductId { get; set; }
                public string Coordinate { get; set; }
                public int VectorId { get; set; }
                public IEnumerable<VectorDatapointDto> VectorDataPoint { get; set; } 
            }

            public class VectorDatapointDto
            {
                public string RefPer { get; set; }
                public string RefPer2 { get; set; }
                public string RefPerRaw { get; set; }
                public string RefPerRaw2 { get; set; }
                public double Value { get; set; }
                public int Decimals { get; set; }
                public int ScalarFactorCode { get; set; }
                public int SymbolCode { get; set; }
                public int StatusCode { get; set; }
                public int SecurityLevelCode { get; set; }
                public string ReleaseTime { get; set; }
                public int FrequencyCode { get; set; }
            }
        }
    }
}
