#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
###############################################################################
#
# pyfind
#
# The python (3.x) implementation of pyfind
#
###############################################################################
"""
import asyncio
import os
import sys

sys.path.insert(0, os.path.abspath(os.path.dirname(__file__)[:-4]))

if __name__ == '__main__':
    from pyfind.__main__ import main
    asyncio.run(main())
