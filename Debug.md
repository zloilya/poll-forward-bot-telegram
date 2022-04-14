Инструкция как какать

я забил и не тестировал в последний раз удачного дебага :)

**Debug**
1) поставить флгаи #define Debug
2) надо запустить ngrok и получить от него прокси сайт
   https://ceea-85-143-144-36.ngrok.io (пример сайта который он выдаст)
   MacBook-Pro% cd /Users/ilyabarishnikov/desktop/haskell/tg-bot/opros-bot
   MacBook-Pro% ./ngrok http 8443
3) теперь надо сделать вебхук, он состоит из 3 частей
   1. это bot + token 
   2. setWebook = сайт ngrok-а
   3. уникальная сылка, делаем нашим токином token
   ~~https://api.telegram.org/bot123456:opatslitblagogithubskazal/setWebhook?uri=https://ceea-85-143-144-36.ngrok.io/123456:opatslitblagogithubskazal~~
   curl --location --request POST "https://api.telegram.org/bot123456:opatslitblagogithubskazal/setWebhook" --header "Content-Type: application/json" --data-raw '{"url": "https://8434-195-93-148-161.ngrok.io/123456:opatslitblagogithubskazal"}'

4) теперь надо запустить сервер
   stack build
   stack run
5) в конце удаляем вебхук
   https://api.telegram.org/bot123456:opatslitblagogithubskazal/deleteWebhook

**Deployed**
1) убедаемся что флаг #define Debug в Server и Security отключен
2) проверям что билдится у себя 
   stack build 
   stack run
3) делаем git push в github и там открываем пулреквест, затем мерджим его
4) скорее всего хероку нас разлогинил, поэтому делаем heroku login
5) как только наши комиты оказались в масетер отправляем их на хероку
   git push heroku

