#!/usr/bin/env python3
# -*- coding: utf-8 -*-
###############################################################################
#
# pyfind
#
# The python (3.x) implementation of pyfind
#
###############################################################################
import asyncio
import os
import sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.realpath(__file__)), '..'))

if __name__ == '__main__':
    from pyfind.__main__ import main
    asyncio.run(main())
