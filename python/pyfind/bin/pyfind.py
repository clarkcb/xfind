#!/usr/bin/env python3.9
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
    if sys.version_info < (3,8):
        sys.exit('Sorry, Python < 3.8 is not supported')

    from pyfind.__main__ import main
    asyncio.run(main())
