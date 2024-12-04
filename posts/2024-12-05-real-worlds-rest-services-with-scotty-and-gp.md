---
title: Real World REST APIs with Scotty and Generic-Persistence
author: Thomas Mahler
tags: haskell, persistence, sqlite, postgresql, database, sql, REST, API, Authentication, bearer token, scotty, generic-persistence,
      
---

<a href="https://github.com/thma/scotty-gp-service"><img src="https://thma.github.io/img/forkme.png" height="20" ></a>


## Abstract

In this blog post I will show how to write a real world REST service in Haskell using the Scotty web framework and the generic-persistence database access library. 

In particular I will demonstrate how to
- build CRUD operations against a database backend, 
- add pagination support
- and secure access with token based authentication

My main motivation for this blog post is to show how compact and readable real world solutions can be written in Haskell. 

## Introduction

Some time ago, I discovered a compact and easy-to-understand article on [how to create a REST service in Haskell with Scotty](https://camunda.com/resources/microservices/haskell/).
The article provides a simple example of a REST service that allows to manage products in an in-memory data structure. The example shows how to use [Scotty](https://github.com/scotty-web/scotty) to define the REST routes and how to use the [aeson](https://github.com/haskell/aeson) library to serialize and deserialize JSON data.

The article was written by Camunda, a company that specializes in modeling and automating business processes.
They see orchestration of microservices as a key use case for their platform and the article provides examples in several programming languages (Java, C#, Python, Go, Typescript and Haskell).

When comparing the Haskell code with the other languages, I found the Haskell code to be the most concise and readable.
That came a bit of a surprise to me, as I had expected that languages like Go, Python or Typescript come with top notch libraries that allow to write REST services in a declarative and compact way.

As I found this langauge comparision based on a simple but practical example quite interesting, I created a [repository with the Haskell code](https://github.com/thma/scotty-service) to invite people to experiment with the code. 

I also contributed back to the authors by providing some improvements to the Haskell code made possible by the GHC2021 language features and some additional ones like `DeriveAnyClass`, `DeriveGeneric`,  `DuplicateRecordFields` and `OverloadedRecordDot`. I also contributed some additional perspectives to the pro and cons section of the article. My suggestinions were well received and the article was updated accordingly.

The article ends with giving some ideas how the example code base could be extended to make it more useful in a real world scenario: 

- Adding token based authentication
- Adding a database backend
- Adding pagination support to the `GET` requests

In this blog post I will show how to implement these features using `scotty`, `wai` and `generic-persistence` libraries. I will not explain the basics setting up Scotty based REST services, as this is already well covered in the above mentioned article.

## Adding a database backend

There are [plenty of options to choose from](https://github.com/Zelenya/elephants) when it comes to Haskell database access libraries. I choose [generic-persistence](https://github.com/thma/generic-persistence#readme) as it aims at minimizing boilerplate code and working in a declarative way. (Being the author of `generic-persistence` I might be biased here, but I think it is a good choice for this example).

### Adding generic-persistence to the project

The first step is to add the library as a dependency to the `package.yaml` file:

```yaml
dependencies:
- generic-persistence
```

### Mapping the data model to the data base

Next we have to enable the Datamodel to be used with `generic-persistence`. This is done by deriving the `Entity` type class for the `Product` datatype:

```haskell
import           Database.GP     (Entity (..))

-- Define a Product data type
data Product = Product
  { id          :: Int,
    name        :: Text,
    description :: Text,
    price       :: Double
  }
  deriving (Show, Generic, ToJSON, FromJSON)

-- Make Product an instance of Entity
instance Entity Product where
  idField = "id"  
```

In order to store a Haskell data type in a relational database, we need to define a mapping between the Haskell type and the corresponding database table. This mapping is defined by the [`Entity`](https://hackage.haskell.org/package/generic-persistence-0.7.0.1/docs/Database-GP-Entity.html) type class. This type class comes with default implementations for all methods which define the standard behaviour. 
This default mapping will work for many cases, but it can be customized by overriding the default implementations.

By default `generic-persistence` would expect the primary key field to be named `productId`. If the primary key field has a different name as in our case, we have to declare it explicitely by defining `idField = "id"`.

Based on the `Entity` instance, `generic-persistence` can generate the necessary SQL queries to interact with the database. For example it will generate the following `CREATE TABLE` statement for the `Product` datatype:

```sql
-- DDL for SQLlite
CREATE TABLE Product (id INTEGER PRIMARY KEY, name TEXT, description TEXT, price REAL);"
```

If you are working with an existing database with deviating table names or column names, you can customize the mapping by providing a custom instance of `Entity` for the datatype.

### Setting up the database connection

To interact with the database in a multi-threaded web server, we can't use a single connection but need a connection pool. The `generic-persistence` library provides a function `createConnPool` to create a connection pool to a database. 
In this example we use a SQLite database, but the library supports other databases as well. setting up a connection pool to a SQLite database is done as follows:

```haskell
-- Create a connection pool to a SQLite db specified by its file path
sqlLitePool :: FilePath -> IO ConnectionPool
sqlLitePool dbFile = createConnPool AutoCommit dbFile connectSqlite3 10 100
```

With this helper function defined we can now create a connection pool to the SQLite database and use it to interact with the database:

```haskell
main :: IO ()
main = do
  -- create a connection pool to the SQLite database
  pool <- sqlLitePool "sqlite.db"
```

### Interacting with the database

Once we have create a connection pool to the database, we can use it in the Scotty actions to interact with the database.


```haskell
main :: IO ()
main = do
  -- create a connection pool to the SQLite database
  pool <- sqlLitePool "sqlite.db"

  -- Start the web server
  scotty 3000 $ do

    -- Define a route to get all products by performing a select query on the Product table.
    get "/products" $ do
      products <- liftIO $ withResource pool $ \conn -> 
        select @Product conn allEntries
      json products
```

The `withResource` function is used to acquire a connection from the connection pool, perform the database operation and release the connection afterwards. 
The `liftIO` is needed to lift the `IO` action into the Scotty action monad `ActionM`.

The signature of the `select` function is `select :: forall a. (Entity a) => Conn -> WhereClauseExpr -> IO [a]`. 

In order to provide the type information for the `select` function, we use the type application syntax `@Product`. 

The `allEntries` value is a predefined `WhereClauseExpr` that does not constrain the returned rows.

In order simplify the code further, we can define a helper function `withPooledConn` that hides the mechanics of acquiring the connection from the pool and the subsequent `liftIO`:


```haskell
main :: IO ()
main = do
  -- create a connection pool to the SQLite database
  pool <- sqlLitePool "sqlite.db"
  -- define Helper function to run a database action with a connection from the pool
  let withPooledConn = liftIO . withResource pool 

  -- Start the web server
  scotty 3000 $ do

    -- Define a route to get all products by performing a select query on the Product table.
    get "/products" $ do
      products <- withPooledConn $ \conn -> 
        select @Product conn allEntries
      json products
```

Now we start writing the other CRUD operations for the `Product` datatype:

```haskell
  
    -- Define a route to get a product by ID
    get "/products/:id" $ do
      productIdParam <- captureParam "id" :: ActionM Int
      prod <- withPooledConn $ \conn -> 
        selectById @Product conn productIdParam
      case prod of
        Just p  -> json p
        Nothing -> raiseStatus status404 "not found"

    -- Define a route to create a new product
    post "/products" $ do
      newProduct <- jsonData
      insertedProduct <- withPooledConn $ \conn -> 
        insert @Product conn newProduct
      json insertedProduct

    -- Define a route to update a product by ID
    put "/products/:id" $ do
      productIdParam <- captureParam "id"
      updatedProduct <- jsonData
      let updatedProductWithId = updatedProduct {id = productIdParam} :: Product
      updated <- withPooledConn $ \conn -> 
        upsert @Product conn updatedProductWithId
      json updated

    -- Define a route to delete a product by ID
    delete "/products/:id" $ do
      productIdParam <- captureParam "id" :: ActionM Int
      deleted <- withPooledConn $ \conn -> 
        deleteById @Product conn productIdParam
      json deleted
```

The `selectById`, `insert`, `upsert` and `deleteById` functions work similar to the `select` function, so there is not much to explain here. Please note how the `generic-persistence` API allows to concentrate on the intented semantics of the operation and hides all the nitty-gritty technical details of data mapping and database operations.

## Pagination of results

Pagination of large result sets is a common requirement for REST services.  
Many relational databases provide support for pagination through the `LIMIT` and `OFFSET` clauses in the `SELECT` statement.
`generic-persistence` provides a `limitOffset` operator that can be used to limit the number of rows returned and to specify the offset position of the first row to be returned. We will use this operator to implement pagination in the `GET /products` route.

When calling `GET /products` we want to be able to specify the page number and the number of records per page as the query parameters of the request. 
So as an example the request `GET /products?page=4&size=20` should return the 20 products starting from the 61st product in the database. 
The response should also include a pagination info that contains:

- the current page number
- the total number of pages
- the total number of records
- the number of the next page if it exists
- the number of the previous page if it exists

a concrete JSON structure could look like follows:

```json
{
  "currentPage": 4,
  "nextPage": 5,
  "prevPage": 3,
  "totalPages": 5,
  "totalRecords": 100
}
```

### A Pagination data type

To represent the pagination info in Haskell, we define a data type `Pagination`:

```haskell
-- Define a Paging information data type
data Pagination = Pagination
  { totalRecords :: Int,
    currentPage  :: Int,
    totalPages   :: Int,
    nextPage     :: Maybe Int,
    prevPage     :: Maybe Int
  }
  deriving (Show, Generic, ToJSON, FromJSON)
```

We also provide a helper function that allows to calculate the pagination info based on the total number of records, the current page and the requested page size:

```haskell
-- | Helper function to build pagination information
buildPagination :: Int -> Int -> Int -> Pagination
buildPagination totalRecords currentPage pageSize =
  let totalPages = (totalRecords + pageSize - 1) `div` pageSize
      nextPage
        | currentPage < 1          = Just 1
        | currentPage < totalPages = Just (currentPage + 1)
        | otherwise                = Nothing
      prevPage
        | currentPage > totalPages = Just totalPages
        | currentPage > 1          = Just (currentPage - 1)
        | otherwise                = Nothing
   in Pagination totalRecords currentPage totalPages nextPage prevPage
```

### Adding pagination to the `GET /products` route

To add pagination to the `GET /products` route, we need to extract the `page` and `size` query parameters from the request and use them to fetch only the matching rows.


```haskell
    -- Define a route to list all products with pagination
    get "/products" $ do
      currentPage <- queryParam "page" `catchAny` (\_ -> return 1)   -- default to 1
      pageSize    <- queryParam "size" `catchAny` (\_ -> return 20)  -- default to 20
``` 

Based on the `currentPage` and `pageSize` we can calculate the offset position of the rows to be returned:

```haskell
      let offset = (currentPage - 1) * pageSize :: Int
```

Using `offset` and `pageSize` we can now perform the select query with the `limitOffset` operator provided by `generic-persistence` in order to select only the records of the requested page:

```haskell
      page <- withPooledConn $ \conn -> 
        select @Product conn (allEntries `limitOffset` (offset, pageSize))
```

Finally we build the pagination info and include it in the output `ProductList` result. We'll have to use the `count` function to get the total number of `Product` records in the database:

```haskell
      totalRecords <- withPooledConn $ \conn -> 
        count @Product conn allEntries
      let info = buildPagination totalRecords currentPage pageSize
      json $ ProductList page info
```

The `ProductList` data type is defined as follows:

```haskell
data ProductList = ProductList
  { 
    products   :: [Product],
    pagination :: Pagination
  }
  deriving (Show, Generic, ToJSON, FromJSON)
  ````

A typical JSON output will look like follows (Assuming that we are calling `localhost:3000/products?page=4&size=3`):

```json
{
  "pagination": {
    "currentPage": 4,
    "nextPage": 5,
    "prevPage": 3,
    "totalPages": 34,
    "totalRecords": 100
  },
  "products": [
    {
      "description": "Description 10",
      "id": 10,
      "name": "Product 10",
      "price": 119.99
    },
    {
      "description": "Description 11",
      "id": 11,
      "name": "Product 11",
      "price": 129.99
    },
    {
      "description": "Description 12",
      "id": 12,
      "name": "Product 12",
      "price": 139.99
    }
  ]
}
```

## Adding token based authentication

`Scotty` is build on top of the `wai` web application interface, which allows to add middlewares to the application. Middlewares are functions that can modify the request and response of the application.
To add token based authentication to the service, we can use the `wai-middleware-bearer` middleware. This middleware allows to extract the token from the `Authorization` header and to validate it against a list of known tokens.

Using such a middleware is a good practice as it allows to separate the business logic of the service (as defined by the Scotty routes) from other concerns like.

- logging
- request validation
- authentication / authorization
- compression
- HTTPS enforcement.

Another good thing about wai middlewares is that they can be composed in a pipeline, so that each middleware can focus on a single concern. And they work independently of the actual web application framework used (like Scotty, Servant and Yesod).

### Adding middlewares to a Scotty application

Adding a middleware to a `Scotty` application is done by using the `middleware` function provided by the library. 
For example we could add a middleware that logs all incoming requests to the console:

```haskell
  -- Start the web server
  scotty 3000 $ do
    -- Add middleware to log all incoming requests
    middleware logStdoutDev
```

This will produce a log output like follows:

```shell
GET /products
  Params: [("page","2"),("size","13")]
  Accept: */*
  Status: 200 OK 0.004865s
POST /products/
  Request Body: {
    "description": "classic pink blue",
    "id": 3,
    "name": "Lava Lamp",
    "price": 499.99
  }
  Accept: */*
  Status: 200 OK 0.00759s
```

### Adding the `wai-middleware-bearer` middleware

To add the `wai-middleware-bearer` middleware we first have to add it as a dependency to the `package.yaml` file:

```yaml
dependencies:
...
- wai-middleware-bearer
```

The package `Network.Wai.Middleware.BearerTokenAuth` provides sevaral functions that can be used to create a middleware that validates the token in the `Authorization` header. For example there is a function `tokenListAuth :: [ByteString] -> Middleware` that takes a list of known tokens and returns a middleware that validates the token against this list:

```haskell
  scotty 3000 $ do
    -- validate token against a list of known tokens
    middleware $ tokenListAuth ["secret", "top-secret"]
```

In a real world scenario the tokens would be stored in a secure way. But even then it could still a bit too static as the tokens would be only loaded once when the application starts.

### Using a custom token validator function

We are looking for a more dynamic way to validate the tokens, that will allow to change the tokens without restarting the application.

`wai-middleware-bearer` supports using custom token validators that can be passed to the middleware by using the `tokenAuth :: TokenValidator -> Middleware` function. 
Here `TokenValidator` is a type synonym for a validation function: `type TokenValidator = ByteString -> IO Bool`. So we will use `tokenAuth` to pass in our custom token validator function to create a middleware.

For this we will provide our own token validator function `validateToken :: ConnectionPool -> TokenValidator` that will be called for each request to validate the token against the database.
In this way we can change the tokens in the database without restarting the application. 

```haskell
validateToken :: ConnectionPool -> ByteString -> IO Bool
validateToken pool token = do
  now <- getCurrentTime                     -- get the current time
  tokens <- withResource pool $ \conn ->    -- use a connection from the pool
    select @BearerToken conn                -- to select tokens from the BearerToken table    
      (field "token" =. token &&.           -- where the token matches, and 
       field "expiry" >. now)               -- expiry is in the future
  return $ not (null tokens)                -- return True if a valid token exists
```

## Summary

In this blog post we have shown how to extend the simple Scotty based REST service example from the Camunda article with a database backend, pagination support and token based authentication.

We have shown that Haskell is a great language for writing real world REST services. The code is concise, readable and easy to maintain. The type system helps to catch many errors at compile time and the GHC language extensions allow to write even more concise code.

As a developer, you simply state your intentions in a declaratice way and delegate all the technical details to the libraries.

I hope you enjoyed this blog post and found it useful. If you have any questions or suggestions, please feel free to contact me.














