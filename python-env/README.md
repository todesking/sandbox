# pyenvで複数のpythonバージョンを管理する

## Pyenvインストール

```shellsession
$ git submodule add git@github.com:pyenv/pyenv.git
$ cd pyenv
$ git checkout v1.2.7
```

## Pythonインストール

```shellsession
$ export PYENV_ROOT="/path/to/pyenv"
$ pyenv/bin/pyenv install 3.5.6
# pyenv/versions/{version} に入る
```

## ライブラリ管理

```shellsession
$ pyenv/versions/3.5.6/bin/pip install numpy
# ...
$ pyenv/versions/3.5.6/bin/pip freeze > requirements35.txt
# pip install -r requirements35.txt でまとめて入る
```
