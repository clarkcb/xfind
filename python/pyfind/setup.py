from setuptools import setup
from pyfind import VERSION

#long_description = open('README.md').read()

setup(name='pyfind',
      version=VERSION,
      description='Python version of xfind',
      # long_description=long_description,
      url='https://github.com/clarkcb/xfind.git',
      author='Cary Clark',
      author_email='clarkcb@gmail.com',
      include_package_data=True,
      install_requires=[],
      license='MIT',
      packages=['pyfind'],
      python_requires='>=3',
      scripts=[
          'bin/pyfind',
          # 'bin/pyfind.bat'
      ],
      tests_require=[
          'nose',
      ])
