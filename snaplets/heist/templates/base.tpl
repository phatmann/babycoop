<html>
  <head>
    <title>Baby Co-op Planner</title>
      <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
      <link rel="stylesheet" type="text/css" href="//netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css"/>
      <link rel="stylesheet" type="text/css" href="//netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap-theme.min.css"/>
      <link rel="stylesheet" type="text/css" href="/screen.css"/>
      <script src="//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js"></script>
  </head>
  <body>
    <nav class="navbar navbar-default" role="navigation">
      <a class="navbar-brand" href="/">Baby Co-op Planner</a>
      <ifLoggedIn>
        <p class="navbar-text navbar-right"><a href="/logout" class="navbar-link">Logout</a></p>
      </ifLoggedIn>
    </nav>
    <div id="content" class="container">
      <ifLoggedIn>
        <h2><coopName/></h2>
      </ifLoggedIn>
      <apply-content/>
    </div>
  </body>
</html>
