<apply template="base">
  <ifLoggedIn>
      <h2><coopName/>: <calendarName/></h2>
      <ul>
        <meetings>
          <li><a href="${meetingURL}">
            <meetingName/>
          </a></li>
        </meetings>
      </ul>
      <p><a href="${otherCalendarURL}"><otherCalendarName/></a></p>
      <div class = "well">
        Any questions, contact <a href="mailto:thephatmann@gmail.com">thephatmann@gmail.com.</a>
      </div>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>
</apply>
