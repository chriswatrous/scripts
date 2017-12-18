#!/usr/bin/env python
import random

N = 16
NUMERIC = '0123456789'
UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
LOWER = 'abcdefghijklmnopqrstuvwxyz'

chars = NUMERIC + UPPER + LOWER

print ''.join(random.choice(chars) for x in range(N))
