<apply template="base">
  <ifLoggedIn>
    <h3><meetingName/></h3>
    <form action="${meetingURL}" method="POST">
      <ul>
        <slots>
          <li>
            <slotPerson/>: 
              <ifSlotViewing>
                <a href="${meetingURL}?edit=${slotPerson}" class="${slotClass}" title="${slotStat}"><slotAttendance/></a>
              </ifSlotViewing>

              <ifSlotEditing>
                <selectAttendance/>
              </ifSlotEditing>
          </li>
        </slots>
      </ul>

      <ifEditing>
        <input type="submit" value="Save Changes" id="submit-button" />
        <input type="hidden" value="${editPerson}" name="person" />
      </ifEditing>

    </form>
    <p><a href="/">Back to calendar</a></p>
    <div class = "well">
      Click on the link next to your name to change your status. Any questions, contact <a href="mailto:thephatmann@gmail.com">thephatmann@gmail.com.</a>
    </div>
  </ifLoggedIn>

  <ifLoggedOut>
    <apply template="_login"/>
  </ifLoggedOut>
</apply>
