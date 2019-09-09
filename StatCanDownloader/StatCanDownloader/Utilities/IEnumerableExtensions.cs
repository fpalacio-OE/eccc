namespace StatCanDownloader.Utilities
{
    using System.Collections.Generic;

    public static class IEnumerableExtensions
    {
        public static IEnumerable<IEnumerable<T>> GetBlock<T>(this IEnumerable<T> source, int size)
        {
            using (var e = source.GetEnumerator())
            {
                while (e.MoveNext())
                {
                    yield return GetBlockElements(e, size - 1);
                }
            }
        }

        private static IEnumerable<T> GetBlockElements<T>(IEnumerator<T> source, int size)
        {
            yield return source.Current;
            for (int i = 0; i < size && source.MoveNext(); i++)
            {
                yield return source.Current;
            }
        }
    }
}
