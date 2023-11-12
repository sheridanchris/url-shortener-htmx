open Giraffe

open Microsoft.AspNetCore.Builder
open System
open System.Data.SQLite
open System.Data.Common
open DbUp
open Donald
open System.Data

module Domain =
  type ShortenedUrl = { Url: string; Slug: string }

  module Slug =
    let createRandom (pickChar: char seq -> char) =
      let length = 6
      let characters = [ 'a' .. 'z' ] @ [ 'A' .. 'Z' ] @ [ '0' .. '9' ]
      [ for _ in 1..length -> pickChar characters ] |> String.Concat

module Database =
  open Domain
  open System.IO

  let private defaultDbPath = Path.Combine(__SOURCE_DIRECTORY__, "local.db")

  let connectionString =
    "SQLITE_CONNECTION_STRING"
    |> Environment.GetEnvironmentVariable
    |> Option.ofObj
    |> Option.defaultValue $"Data Source={defaultDbPath}"

  let createConnection () = new SQLiteConnection(connectionString)

  let private readShortenedUrl (reader: DbDataReader) =
    let url = reader.ReadString "url"
    let slug = reader.ReadString "slug"
    { Url = url; Slug = slug }

  let doesSlugExist (connection: IDbConnection) (slug: string) =
    connection
    |> Db.newCommand "select exists(select slug from urls where slug = @slug)"
    |> Db.setParams [ "slug", SqlType.String slug ]
    |> Db.Async.scalar (fun result -> (result :?> int64) = 1)

  let getShortenedUrl (connection: IDbConnection) (slug: string) =
    connection
    |> Db.newCommand "select url, slug from urls where slug = @slug"
    |> Db.setParams [ "slug", SqlType.String slug ]
    |> Db.Async.querySingle readShortenedUrl

  let saveShortenedUrl (connection: IDbConnection) (shortenedUrl: ShortenedUrl) =
    let parameters = [
      "url", SqlType.String shortenedUrl.Url
      "slug", SqlType.String shortenedUrl.Slug
    ]

    connection
    |> Db.newCommand "insert into urls (url, slug) values (@url, @slug)"
    |> Db.setParams parameters
    |> Db.Async.exec

module Views =
  open Giraffe.ViewEngine
  open Giraffe.ViewEngine.Htmx

  let template (content: XmlNode) =
    html [] [
      head [ _title "URL Shortener" ] [
        Script.minified
        link [
          _rel "stylesheet"
          _href "https://unpkg.com/missing.css@1.1.1"
        ]
      ]
      body [] [ content ]
    ]

  let errorNotice (error: string) =
    div [
      _id "notice"
      _class "box bad"
    ] [
      strong [ _class "block titlebar" ] [ str "Error" ]
      str error
    ]

  let shortenUrlPartial =
    main [] [
      div [ _id "notice" ] []
      form [
        _hxPost "/shorten"
        _hxSwap "outerHTML"
        _hxTarget "closest main"
      ] [
        fieldset [] [
          legend [] [ str "Shorten a URL" ]
          p [] [
            label [ _for "url" ] [ str "Enter a URL" ]
            input [
              _id "url"
              _name "url"
              _placeholder "https://site.com"
              _type "url"
              _required
            ]
          ]
          p [] [
            label [ _for "slug" ] [ str "Enter a custom slug (optional)" ]
            input [
              _id "slug"
              _name "slug"
              _type "text"
            ]
          ]
          p [] [ button [ _type "submit" ] [ str "Shorten!" ] ]
        ]
      ]
    ]

  let shortenedUrlPartial (createUrlFromSlug: string -> string) (shortenedUrl: Domain.ShortenedUrl) =
    main [] [
      p [] [
        label [ _for "url" ] [ str "Original URL" ]
        input [
          _id "url"
          _readonly
          _value shortenedUrl.Url
        ]
      ]
      p [] [
        label [ _for "short-url" ] [ str "Shortened URL" ]
        input [
          _id "short-url"
          _readonly
          _value (createUrlFromSlug shortenedUrl.Slug)
        ]
      ]
      p [] [
        // this should probably be an <a href="/">Shorten Another!</a>
        button [
          _hxGet "/"
          _hxSwap "outerHTML"
          _hxTarget "closest main"
        ] [ str "Shorten Another!" ]
      ]
    ]

module Handlers =
  open Domain
  open Giraffe.Htmx
  open Microsoft.AspNetCore.Http

  [<CLIMutable>]
  type CreateShortenedUrlCommand = { Url: string; Slug: string }

  let private pickCharacter (characters: char seq) =
    let length = Seq.length characters
    let randomIndex = Random.Shared.Next(0, length)
    Seq.item randomIndex characters

  let private visitUrlFactory (request: HttpRequest) : string -> string =
    sprintf "%s://%s/url/%s" request.Scheme (string request.Host)

  let htmxView content : HttpHandler =
    fun next ctx ->
      let view =
        if ctx.Request.IsHtmx && not ctx.Request.IsHtmxRefresh then
          content
        else
          Views.template content

      htmlView view next ctx

  let shortenUrl (command: CreateShortenedUrlCommand) : HttpHandler =
    fun next ctx ->
      task {
        use connection = Database.createConnection ()

        let slug =
          if String.IsNullOrWhiteSpace command.Slug then
            Slug.createRandom pickCharacter
          else
            command.Slug

        let! slugExists = Database.doesSlugExist connection slug

        if slugExists then
          let notice = Views.errorNotice "A shortened url with that slug already exists!"
          return! (withHxRetarget "#notice" >=> htmlView notice) next ctx
        else
          let shortenedUrl: ShortenedUrl = { Url = command.Url; Slug = slug }
          do! Database.saveShortenedUrl connection shortenedUrl

          return!
            shortenedUrl
            |> Views.shortenedUrlPartial (visitUrlFactory ctx.Request)
            |> htmlView
            |> fun view -> view next ctx
      }

  let visitShortenedUrl (slug: string) : HttpHandler =
    fun next ctx ->
      task {
        use connection = Database.createConnection ()
        let! shortenedUrl = Database.getShortenedUrl connection slug

        let respondWith =
          match shortenedUrl with
          | None -> setStatusCode 404 >=> text "A shortened url with that slug was not found!"
          | Some shortenedUrl -> redirectTo true shortenedUrl.Url

        return! respondWith next ctx
      }

  let router: HttpHandler =
    choose [
      route "/" >=> GET >=> htmxView Views.shortenUrlPartial
      routef "/url/%s" (fun slug -> GET >=> visitShortenedUrl slug)

      route "/shorten"
      >=> POST
      >=> bindForm<CreateShortenedUrlCommand> None shortenUrl

      setStatusCode 404 >=> text "Not found!"
    ]

DeployChanges.To
  .SQLiteDatabase(Database.connectionString)
  .WithScriptsFromFileSystem("Migrations")
  .Build()
  .PerformUpgrade()
|> ignore

let builder = WebApplication.CreateBuilder()
builder.Services.AddGiraffe() |> ignore
let app = builder.Build()
app.UseGiraffe Handlers.router
app.Run()
