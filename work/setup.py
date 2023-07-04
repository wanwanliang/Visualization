from setuptools import setup


def readme():
    with open("README.md") as f:
        return f.read()


setup(
    name="SEC",
    version="0.1",
    description="Stress and Environment characterization",
    author="Wanwan Liang; Joseph Sombeck",
    author_email="wanwan.liang@syngenta.com, joseph.sombeck@syngenta.com",
    keywords="GxE analysis, Stress characterization, Environment characterization",
    packages=["SEC"],
    install_requires=['scikit-learn' , 'statsmodels', 'asyncio', 'aiohttp', 'nest_asyncio',
                      'pandas','MiniSom', 'numpy', 'kneed', 'hyperopt'],
     classifiers=[
        'Programming Language :: Python',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: 3.10'
    ],
    dependency_links=[
        'https://mirror.syngentaaws.org/pypi/simple/'
    ],
    include_package_data = True,
    package_data = {'': ['data/*.csv']}
)
