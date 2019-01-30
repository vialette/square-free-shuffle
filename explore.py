import itertools as it

def chunker(iterable, chunksize):
    """
    Return elements from the iterable in `chunksize`-ed lists. The last returned
    chunk may be smaller (if length of collection is not divisible by `chunksize`).

    >>> print list(chunker(xrange(10), 3))
    [[0, 1, 2], [3, 4, 5], [6, 7, 8], [9]]
    """
    i = iter(iterable)
    while True:
        wrapped_chunk = [list(it.islice(i, int(chunksize)))]
        if not wrapped_chunk[0]:
            break
        yield wrapped_chunk.pop()

def guess(s):
    n = len(s)

    if n % 4 != 0:
        return

    for chunk_size in range(4, 1 + n // 4):
        if n % chunk_size == 0:
            chunks = ["".join(c) for c in chunker(s, chunk_size)]
            tentative = list(set(chunks))
            if len(tentative) == 4:
                print(tentative)
