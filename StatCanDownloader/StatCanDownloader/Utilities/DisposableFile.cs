namespace StatCanDownloader.Utilities
{
    using System;
    using System.IO;

    public class DisposableFile : IDisposable
    {
        public string Path;

        public DisposableFile(string path) => Path = path;

        public void Dispose()
        {
            File.Delete(Path);
        }
    }
}
