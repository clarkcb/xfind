from setuptools import setup
from pyfind import __version__

#long_description = open('README.md').read()

setup(name='pyfind',
      version=__version__,
      description='Python version of xfind',
      # long_description=long_description,
      url='https://github.com/clarkcb/xfind.git',
      author='Cary Clark',
      author_email='clarkcb@gmail.com',
      include_package_data=True,
      install_requires=[],
      license='MIT',
      packages=['pyfind'],
      package_data={'': ['data/*.json']},
      python_requires='>=3.7',
    #   scripts=[
    #       'bin/pyfind.sh',
    #   ],
      tests_require=[
          'nose',
      ])
