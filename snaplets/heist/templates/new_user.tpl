<apply template="base">
  <h1>Register a new user</h1>

  <form method="post" action="/new_user">
    <table id="info">
      <tr>
        <td>Calendar:</td><td><input type="text" name="calendar" size="60" /></td>
      </tr>
      <tr>
        <td>Login:</td><td><input type="text" name="login" size="20" /></td>
      </tr>
      <tr>
        <td>Password:</td><td><input type="password" name="password" size="20" /></td>
      </tr>
      <tr>
        <td></td>
        <td><br/><input type="submit" value="Add User" /></td>
      </tr>
    </table>
  </form>
</apply>
