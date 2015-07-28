---
title: Solving the false alert bug in ‘Panic Button’
slug: 2015-05-18-panic-button-trigger
date: 2015-05-18
collection: news
format: tech
tags: amnesty international, panic button
projects: panic-button
published: false
---

To help solve the issue we tried to take a rigorous approach to modeling the trigger mechanism and the potential events that created false alerts. We took the approach that if we could see the problem and visualise its moving parts, then we could understand it and make better decisions. To complete the picture, we used a simple spreadsheet with a custom function to take a "data-driven" approach to changing the parameters. In a way, it's a simple but accurate model of several ways that the panic button triggering mechanism works, which allows to explore different options and understand their impact on known "test cases". 

<!--more-->

To a large extent we had only anectodal data about the issue. Having some users that closely worked with us to tell us when the false alert would occur helped a lot in creating hypotheses that helped us create a model of the problem. Given our concern for privacy, with adopting minimal data collection and with requesting minimal permissions on the Android app, we didn't want to roll out a data logging component on the public version of the app. If we run into further issues down the road, we plan to release a stripped down "trigger logging" version of the app which would be installed independently of Panic Button and would help collect data about screen on off events on a broad range of phones and for a broad range of users.

Here’s a way to simplify what’s going on. With multiple unintended clicks, sometimes one of the clicks ends up in the “confirmation window”, which is (in our current test version) between 8 secs (6 sec trigger time + 2 sec guard time) and 11 sec (+ 3s confirmation time).

This might happen in two ways:
 - Long blinky event - a notification event which lasts longer than 8 seconds (probably rare)
 - Blinky event plus User event - a notification event creating an initial trigger which then waits for a confirmation, then the user picks up the phone and presses the power button within the confirmation window.

So far we’ve increased the number of clicks needed before the trigger time and shorten the trigger time. However, this doesn’t seem to be solid enough because the false alert patterns are rapid clicks which sometimes might be continued beyond 8 secs, or probably more frequently the user actually acts during this confirmation window - by picking up their phone or pressing the power button.

So one of the natural things to try is to extend the guard time (to 5 secs), but maybe more importantly, try to communicate to the user that the guard time is occurring. This should achieve a few things:
 - It should limit even further the possibility for a completely automated unintended trigger to occur, because such an event would need to occur over a much longer time, 11 seconds (6 sec trigger time + 5 sec guard time) to reach the “confirmation window”
 - In the case where a user action would generate the confirmation click without thinking about it (for instance, if the user picks up the phone shortly after - after the automated event occured) then their actions would be ignored during a longer time (5 seconds while in the guard time instead of 2 secs).
 - Once the user realises that the phone is vibrating with an unusual pattern, they have the chance to stop fiddling with their phones until the confirmation window is over (11 sec + 3 seconds).
 - In case they still actually trigger the alert, they will feel a vibration pattern that will communicate more obviously that the alert has started. Giving them a chance to cancel it, or making them reassured in an emergency situation that it has been correctly activated.

The question becomes, how to communicate to the user? I suggest that we use a vibration pattern that has it's "signature" and helps trigger "haptic memory" by its recognisable nature. Simply repeating a short vibration every seconds 4 times ("straight 4s") seems like a natural starting point. The problem is that it might not be recognisable enough, or distinguisable from a regular notification vibration. In the case where the phone would be lying on a table and would attract the attention of the user that way, this "straight 4s" pattern might not stand out. I suggest that a countdown pattern will more easily evoke "alert"  in the user's mind. The pattern I propose we try would be 4 short vibrations on the first second. 3 vibration on the next, then 2 then 1.

Finally, I also think we should then communicate to the user about the result of either action or inaction (or false manipulation). I propose that we create a much more obvious activation vibration pattern that makes it unmistakeable that the alert has started. I think there are benefits in helping people mitigate the problem if false alerts start sending (despite our efforts to make this even more unlikely) but maybe more importantly, it should help provide a reassuring signal to the user that everything worked as planned when they're in an emergency. I suggest we use the multiples of 4 pattern again, with 4 long vibrations. Conversely, if the confirmation has not been triggered, then 3 short bursts should help signal the user that it's now safe to use their power button.

Here's what the user experience would feel like as a consequence:

 - Voluntary trigger:

    - I press multiple times on the power button in my pocket.
    - I feel the Guard time pattern vibrations (which bring back a reassuring haptic memory from the wizard)
    - I realise that Panic Button is waiting for a confirmation and I wait until the Guard time pattern is finished.
    - I then press the power button once more to confirm.
    - I feel the 4 long vibrations which confirms that the alert is activated.
<br><br>
 - Avoiding false alert:

    - I pick up my phone at the other end of the room after I received a notificaiton.
    - I feel the Guard time pattern vibrations (which bring back the haptic memory)
    - (possibly) I rush into clicking but because I'm inside the guard time, the alert doesn't trigger.
    - I realise that Panic Button is vibrating and is therefore waiting for a confirmation.
    - I wait until I feel the "cancel" vibration.

Ideally after confirming that this doesn't cause a problem we would run a test within our larger pilot groups and ask for feedback. Maybe also run tests like you guys did with Bernard asking random potential users to achieve a task.

There might be another pattern to consider which would require to change the trigger mechanism even more, which might have some implications on how easy it is to learn how to trigger and for users who already got trained to learn the new mechanism. This pattern would transform the guard window into a cancel window. In this scenario, there would be also a 5 secs vibration pattern, but the user would not have to confirm at the end of it. Instead not doing anything will trigger the alert. Adding a press during this "cancel window" will "abort" the coutdown and not trigger the alert. 

Finally, what we also need is more data. Because of the privacy and connectivity implications of adding a "logging" piece of code in the released APK, I think that we should create a stripped down version of the APK with only the power button receiver and a log collection server (we used a couple for debuggin purposes) and ask folks who would want to take part in our "bug chase" to help use map the behavior of phones out there by accepting to submit to us periodic anonymous reports of vibration patterns of android phones. We could make a nice data visualisation out of it and maybe find a way to incentivise users to participate with a clever communication strategy... ("help us make panic button work for the most people possible"?)

Cheers,

Jun

