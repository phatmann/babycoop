<apply template="base">
  <ifLoggedIn>
      <h2>Seattle League of Awesome Moms Baby Co-op (SLAM)</h2>
      <ul>
        <meetings>
          <li><a href="${meetingURL}">
            <meetingDate/>
          </a></li>
        </meetings>
      </ul>
      <p><a href="/?past=yes">Past meetings</a></p>
      <div class = "well">
        Any questions, contact <a href="mailto:thephatmann@gmail.com">thephatmann@gmail.com.</a>
      </div>
    <p><a href="/logout">Logout</a></p>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>
</apply>
