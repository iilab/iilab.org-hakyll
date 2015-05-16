_This is the first part of a series on tools for open collaboration. This first part will focus on managing content in an open way with collaborative editing platforms. In the next posts we'll look at file sharing and document management, task management and communication tools._

<!--more-->

One way I like to procrastinate is by trying to find betters tools. When things aren't as smooth and natural as I would expect, then after a while I go for a round of search and trials, fill up comparison spreadsheets - like for [project management software](https://docs.google.com/spreadsheet/pub?key=0AhQdlH5mVj3PdGE3bVQ4amtxcWdLUFMwYXpnb0NSamc&single=true&gid=0&output=html) - or think about how to build tools to compare tools better (no [really](https://github.com/iilab/openintegrity/issues/34)). I find it edifying to see how different people think differently about organising the way they work. Well that's my justification anyway, sometimes it's just me spending too much time _thinking about doing_ rather than _doing_... 

Our team is tech savvy, willing to experiment and push the boundaries of how we do things. For instance, with the Open Integrity index, we've tried to be [as open as possible](https://wiki.openintegrity.org) during the implementation of the project. In her art and writing, Kat is a supporter of Open Access publishing and Creative Commons. Sam is completely dedicated to the principles of sharing, such as his [log about his time at Open Source Ecology](http://opensourceecology.org/wiki/Samthetechie_log), with the aim of allowing to build on top of what he's done. 

##  In the world of Open practices

As when I start any new project, I've been researching the best ways to work together openly for Droplet. I've used different approaches for different projects, both to try them out and to cater for different audiences and teams. There are some great experiments out there to learn from: from the more business inspired to the more community driven. 

Gittip's founder - also one of the initiators of the (Open Company Initiative](http://www.opencompany.org/about/) - made it a practice to [hold partner calls "on air"](https://medium.com/building-gittip/5886749a4ded). There are also some formal methodologies out there to implement an Open Design process like the [Open P2P Design Toolkit](http://issuu.com/openp2pdesign/docs/openp2pdesign.toolkit_pixelache) - "a simple tool for helping people approach design thinking on a metadesign level". Sounds simple to me! Though it would be nice to see the source for the toolkit on github.... And the [Open Design Definition](https://github.com/OpenDesign-WorkingGroup/Open-Design-Definition/blob/master/open.design_definition/open.design.definition.md) which however probably will focus on [the nature of the knowledge shared rather than the processes that enable it](https://github.com/OpenDesign-WorkingGroup/Open-Design-Definition/issues/17).

## Putting it in motion

Droplet [aims to adhere to the principles](https://www.penflip.com/jun/droplet/blob/master/open-design.txt) of the [current version (v0.3)](https://github.com/OpenDesign-WorkingGroup/Open-Design-Definition/blob/03c2d543242fed0d8e999b79d96c6671b46406a6/open.design_definition/open.design.definition.md) of the Open Design Definition and in particular commits to ensure that the "source documentation is made publicly available so that anyone can study, modify, distribute, make, prototype and sell the artifact based on that design." So then what about our processes and tools? 

There's a spectrum of choices out there from using "ready made" tools that are proprietary and cost money or to integrate various existing open source components to achieve desired functionalities. I'll mention both approaches, but often in order to test if a process is the right one, it's good to be up and running fast and online platforms can be a good way to do that. You can then migrate to open source tools when you know the process works for you.

## Managing Text

For us text means rich text, our design documents, engineering documents, blog posts but also sometimes spreadsheet data like our budget data. Allow a team to work together on text during various moments in the project - when face to face, in a call, when we're working separately - is a way to make sure you capture things as you go along and that people are _on the same page_. 

Real-time collaborative editing is a good way to capture the important elements of a conversation and have a easy to access place where everyone can jot down their ideas without interrupting the conversation. In fact sharing links and pictures can help enhance a conversation as long as participants keep the right balance between face time and screen time.

![Distraction free and live formatting with Folding Text](http://www.foldingtext.com/static/gallery/1-simple.png)

We like Markdown and the alternate flavors of markdown because they allow you to write your formatting without interrupting your writing of content. I particularly like interfaces which format your text live - such as [Penflip](https://www.penflip.com), but also [FoldingText](http://www.foldingtext.com/), [Task paper](www.hogbaysoftware.com/products/taskpaper) is also an interesting variation on live formatting for task management - and I find them much better than interfaces that require to click on buttons to achieve the same goals, so while we started on google docs (because of the Google Drive integration, the Spreadsheet and the new Track Changes add-on) we've been looking into other approaches. 

We also need versioning and commenting and ways to allow the public to contribute in these ways. Ideally we want to blur the lines between content publishing and collaborative documentation where various circles in the community contribute and have the permission to "publish" (which might mean the same thing as merging or accepting pull requests).

In summary the features we are looking for are:

 - Realtime collaborative editing
 - Distraction free writing
 - Inline (live) formatting
 - Markdown
 - Versionning (Git)
 - Commentting

### Open Source Tools 

 - Etherpad-lite (and its plethora of [less known plugins](http://mclear.co.uk/2014/01/04/top-10-etherpad-plugins-2014/))
 
 ![The Page View plugin for Etherpad-lite. Looks familiar?](http://mclear.co.uk/files/2014/01/crop-550x159.png)
 
 - [Ethercalc](http://ethercalc.net/) - pictured below - is a bit clunky but improving over time, and the promising [Ethersheet](https://ethersheet.org) that still lacks features.
 
 ![Ethercal is simple and not very pretty...](/images/news/screen-shot-2014-06-03-at-215510.png)

 - [Substance](http://substance.io/composer/) shifted its focus to desktop document editing (when originally it was also looking at realtime web based collaboration. But I keep an eye on this one because its [technology fundamentals are great](https://github.com/substance)! 

 ![Substance is a beautifully designed approach to writing text](http://substance.io/images/screenshots/substance-composer-beta-3.png)

### Online Services

  - [Penflip](https://www.penflip.com) - which this blog post was prepared on, but doesn't allow for live editing by multiple users. However it allows to completely open text to external collaboration. It has "git" at its core and therefore also allows to do version control and offline working. It has some UX problems however, for instance comments are only attached to request for modifications of the text. Also saving is a two step process (committing and pushing) which can be confusing and can be forgotten.

 ![This blog post being edited on Penflip](/images/news/screen-shot-2014-06-03-at-220144.png)
  
  - [Typewrite](https://typewrite.io/) - which does allow live editing but doesn't have document folders like Penflip, and doesn't allow to comment or integrate with a git workflow.

 ![Typewrite looks good and distraction free but lacks integration options](/images/news/screen-shot-2014-06-03-at-220525.png)

  - [Stackedit](https://stackedit.io/) - which seems promising, connects to Google Docs and Dropbox while still using Markdown. Is able to publish to github. We might try this next since Penflips's overall UI is not fantastic.
  
 ![Stackedit is a bit cluttered and inelegant but is feature rich](/images/news/screen-shot-2014-06-03-at-221941.png)
  
# Conclusion

I feel that the tools in this space have been changing fast in the past year. I think that organisations need to invent the processes that will work to make the most of them. I've been told before that the most important is to just put your code out there. You can't claim to be "wanting to be open source" and have nothing out there for others to look at. There is a shyness and counter-intuitive thing about putting your unfinished work out there, but I find it's a healthy practice, very much worth the effort. To be honest, we can't claim to have any open source project that has garnered enough attention for it to reap the benefits of the collective intelligence and cognitive surplus pouring our way. But [Panic Button](https://panicbutton.io) might become that, and hopefully Droplet and our other projects too. Until then, it just puts the right sort of pressure on us to deliver better work and think more about how to engage with contributors beyong traditional boundaries of organisations, roles and status.

# Your thoughts

We'd really love to hear from you about your thoughts on open collaboration. Have you got a project where you're trying similar things out? Is there anything you think we could improve on? At the moment the best way to do that is to comment here on the blog, [submit a change to this blog post to improve it](https://www.penflip.com/jun/droplet-blog-posts/blob/master/02-Tribulations-in-Open-Collaboration.txt) or contact us through contact@iilab.org - but we're going to continue to try to improve on those means too!

### Other Resources

 - A very good [in depth post from Zapier about collaborative writing tools](https://zapier.com/blog/collaborative-writing-tools-editorially-draft-penflip/)
 - [AlternativesTo ](http://alternativeto.net/) is a good site for finding open source alternatives to the software you already use.
 - [Social Source Commons](https://socialsourcecommons.org/) from our friends at Aspiration Tech
 - Gittip has [published a list of the tool they use](http://building.gittip.com/appendices/channels).

