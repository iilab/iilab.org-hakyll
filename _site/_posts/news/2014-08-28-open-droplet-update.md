Open Droplet is coming along nicely after a short summer break. [Alex Shure](http://etemu.com/) has joined in working on Open Droplet and the project has taken the direction of joining the [Open Energy Monitor](http://openenergymonitor.org/emon/) ecosystem. 

<!--more-->

Our first [investigations into using piezos and accelerometres for measuring water usage](https://iilab.org/news/2014-05-23-getting-started-droplet.html), combined with a few experiments, led us to question the most feasible method of measuring flow in domestic settings. We've now decided to move towards a scaled series of prototypes, with the first responding automatically to flow, but not directly measuring the volume of water discharged. 

![Designing the Power Supply component](/images/news/droplet-whiteboard.jpg)
_Designing the Power Supply component_

The water usage is then determined (after calibration) by the length of time for which the flow is detected... Which is to say, the first iteration of Open Droplet will be a bit like [the timers we mentioned](https://iilab.org/news/2014-06-09-lay-of-the-ocean.html) in a previous post - but below you'll see how Open Droplet incorporates an automatic trigger, and will be networked so users can keep a record of their water use. In terms of networking, we're looking at incorporating a hub (building on the Open Energy Monitor architecture) to store and relay information to the network.

![Microphone, OpAmp and our new Ersa i'CON PICO soldering station](/images/news/droplet-mike.jpg)
_Microphone, OpAmp and our new Ersa i'CON PICO soldering station_

We are now in the middle of a 2 week development and experimentation period where Alex and Sam are doing wizardry with the hardware. The guys are using a microphone to detect water flow - it starts recording when it registers sustained noise at the right frequencies. It also saves power by only activating the relevant circuits after detecting flow, and will switch off when things quieten down in the bathroom.

For supplying power, we have plug and play battery charging system that can charge with the mini-USB plug (like you'd use for a phone or camera). It uses a linear regulator and an analogue system on a chip to control the charging rate. For the first prototype we're using a phone battery (1S1P 1600mAh LiPo cell). We have been having discussions around the pay-off between the charging time and the amount of heat generated, and have sensibly decided on safety first - ie: a longer charging time, to avoid any (probably very exciting but maybe damaging) explosions. 

![Our Power System prototype](/images/news/droplet-charging.jpg)
_Our Power System prototype: plug and play battery charging, linear regulator, analog SOC binary output, 1S1P 1600mAh LiPo cell_

The next hurdle is around making the prototype waterproof, for which we're thinking of encasing it in either epoxy of polyurethane by pouring the mixture directly onto the kit. We'd then have just the one weak n' wet point - the usb connector for power - which we'd plug with a winged rubber cap. If that doesn't work, we have a back-up plan involving screws, but more on that if we need to go there. 

With encasing come a number of problems, not least overheating. So we're going to be testing the two materials for acoustic and heat conductance in the first instance - and resilience to dropping, of course. 

![Elements of our workspace](/images/news/droplet-shelf.jpg)
_Elements of our workspace_

![Putting the prototype together](/images/news/droplet-prototype.jpg)
_Putting the prototype together_

