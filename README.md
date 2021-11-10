# Erlang service template [![Build Status](http://ci.rbkmoney.com/buildStatus/icon?job=rbkmoney_private/erlang-service-template/master)](http://ci.rbkmoney.com/job/rbkmoney_private/view/Erlang/job/erlang-service-template/job/master/)


Шаблон проекта для быстрого старта написания сервиса на Erlang, в комплекте:

 - rebar.config
 - release
 - Jenkinsfile
 - Dockerfile template
 - docker-compose config template
 - documentation stubs
 - common test stub
 - elvis ruleset

Чтобы опробовать в деле, надо всего лишь:

```bash
$ mkdir -p ~/.config/rebar3/templates
$ cd $_
$ git clone https://github.com/rbkmoney/erlang-templates.git .
$ cd ~
$ rebar3 new erlang-service name=snakeoil
$ cd snakeoil
$ git init
$ git submodule add -b master https://github.com/rbkmoney/build_utils.git build_utils
$ git submodule init
...
+ установить значения переменных BASE_IMAGE_TAG и BUILD_IMAGE_TAG в Makefile
```

> _Хозяйке на заметку_. Для того, чтобы это всё заработало, необходимы следующие компоненты:
>
>  * GNU Make
>  * [rebar3](http://www.rebar3.org/)
>  * [elvis](https://github.com/inaka/elvis/releases)
>
> К счастью, теперь все это доступно в build образе, работать с которым легко и удобно через `make`:
>  * wc_<target> - запустить в build контейнере
>  * wdeps_<target> - запустить в build контейнере вместе с необходимыми зависимыми сервисами, используя `docker-compose`.
>  * build_image - построить образ микросервиса
>  * push_image - отправить образ микросервиса в docker registry

Чтобы получить описание и поддерживаемые переменные:

```bash
$ rebar3 new help erlang-service
erlang-service:
    custom template (~/.config/rebar3/templates/erlang-service.template)
    Description: Erlang OTP Service
    Variables:
        name="myapp" (Name of the service, an application with this name will also be created)
        description="A service that does something" (Short description of purpose of the service)
        version="1" (Initial version string)
        date="2016-01-26"
        datetime="2016-01-26T14:17:58+00:00"
        author_name="Author Name"
        author_email="a.name@corporate.domain"
        copyright_year="2016"
        apps_dir="apps" (Directory where applications will be created if needed)
```

В случае необходимости доработки в первую очередь обращайтесь к [официальной документации](http://rebar3.org/docs/tutorials/templates/).

## Внесение изменений

В случае, если необходимо изменений самих модулей (`.erl`), для использования автоматического форматирования с помощью `erlfmt` (`rebar3 fmt`) требуется временное изменить/удалить строчки с mustache-плейсхолдерами.

Например, из-за следующей строки из <file:service-templates/apps/app/test/app_tests_SUITE.erl>:
```erlang
-module({{name}}_tests_SUITE).
```
`erlfmt` не сможет отформатировать файл в частности и проект до конца в целом.

Временная замена `{{name}}_tests_SUITE` на, например, `name_tests_SUITE` позволит обойти эту проблему.
