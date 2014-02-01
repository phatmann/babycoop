<apply template="base">
  <h1>Register a new coop</h1>

  <form method="post" action="/new_coop">
    <table id="info">
      <tr>
        <td>Co-op Name:</td><td><input type="text" name="name" size="60" /></td>
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
