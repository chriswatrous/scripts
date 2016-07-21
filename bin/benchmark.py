from time import time

while True:
    start = time()
    i = 0
    while i % 10000 != 0 or time() < start + 1:
        i += 1
    print '{:,}'.format(i)
