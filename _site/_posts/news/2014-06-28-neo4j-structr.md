A few weeks ago I went to the neo4j meetup at c-base in Berlin. I met Michael Hunger who was hosting the day and is a charismatic presenter and knowledgeable techie. I recommend taking a look at his blog [Better Software Development](http://jexp.de/blog/) for some practical examples about neo4j. This was really instrumental in defining our technology stack for the [Open Oil data framework project](/projects/open-oil-framework.html).

<!--more-->

## Open Oil Tech choices

I've been following graph databases and semantic web technologies for a while but I finally have the occasion to work on a project that uses these technologies which is really meaningful and ambitious. With [Open Oil](http://openoil.net) we are building on the work of [Open Corporates](https://opencorporates.com) around identifying corporations and mapping corporate ownership structures and plan to extend it in several ways. Open Oil focuses on the **Oil and Gas industry** vertical. In this sector, we want to add corporate data in areas where official registries are not easily accessible (like with our Nigeria data set, contributed by local organisations on the ground), map in detail the corporate structure of giants (such as BP), but also extend the mapping beyond corporate structures and into **supply chain networks** and **influence networks**.

To get started on tech architecture, we used [Jeff Patton's User Story Mapping methodology](http://www.agileproductdesign.com/presentations/user_story_mapping/) first to delineate the key functional components of the project and ensure that we would deliver value to the different Open Oil projects right from the start. We then worked [on the data model](https://www.penflip.com/jun/iilab-graph/blob/master/Architecture-and-Frameworks-.txt#data-model). Open Corporates has done an amazing job documenting their own data model both with [their API documentation](http://api.opencorporates.com/documentation/API-Reference) and [a great blog post by Seb Bacon](https://blog.opencorporates.com/2014/01/08/understanding-corporate-networks-part-4-how-we-record-the-data/) which goes into the subtleties of their Statement and Provenance model.

![](https://opencorporates.files.wordpress.com/2014/01/schema1.png)

It seems clear that given how the big picture of how the Open Oil data and Open Corporate data is meant to co-evolve, we need to pay very close attention to how the data models relate to each other. More than that, I think that in the future we need to embrace linked open data and an [open world assumption](http://semanticweb.com/introduction-to-open-world-assumption-vs-closed-world-assumption_b33688) on corporate data, ownership networks, supply chain and influence mapping. This means that we'll probably need technology to do federated queries or more complex graph analytics on top of a distributed linked data store. As far as I'm aware, this is not yet ready for prime time.

For now, we decided to adopt a simplified model of provenance, postpone the adoption of the Statements model (which I think would necessitate to adopt triplestore technology) and keep loose but meaningful interoperability with Open Corporates, use neo4j as our graph database and Structr as an application framework.

## Why neo4j ? Why Structr?

I think that triplestore technology and Linked Data is really the future of data. However, it's been gestating in academia for dozens of years and is only slowly maturing into being able to withstand real-world applications that can be built with a small team in a short amount of time. Folks like Leigh Dodds and Ian Davis with their [Linked Data Patterns](http://patterns.dataincubator.org/book/) or Jeni Tennison with her [blog which is a great resource](http://www.jenitennison.com/blog/) full of her experience with applying linked data to UK open government data projects.

I'm sure that v1.0 of the Open Oil data framework will be built on these technologies, but with short deadlines and in the spirit of building something useful as soon as possible, we went to the vibrant neo4j community. What sealed the deal for me was when Michael Hunger mentioned this project Structr, a CMS built on neo4j that was evolving into an application framework. Looking at the demo, test driving it, seeing how responsive the development team was ([Christian](https://github.com/cmorgner) and [Axel](https://github.com/amorgner) kudos!) confirmed that this was the right choice. That is even with some worries about backtracking on integrating neo4j 2.1 because of the data corruption issues (that were fixed in 2.1.2) and the fact that the framework is very young. 

## Building the Data Model

In starting to build the data model for the open oil data framework, I was reminded of a conversation with [Alvaro Graves](https://twitter.com/alvarograves), who told me on the Thames, aboard a boat between Moz Fest 2013 and the closing party in central london, that the key with linked data technology is that the schema is not separated from the data. We also had a nice chat about how to make statements about statements (which at the time interested me in relation to the [News Verification Ecosystem project](https://groups.google.com/forum/#!forum/news-verification-standard)). The realm of [reification, quads or named graphs](http://patterns.dataincubator.org/book/reified-statement.html)... which I'll probably talk about in another post.

I've posted my [notes about relevant information from Open Corporates about their data model](https://www.penflip.com/jun/iilab-graph/blob/master/Architecture-and-Frameworks-.txt#data-model) and [links and notes to the first version of our data model](https://www.penflip.com/jun/iilab-graph/blob/master/v0.1a) which is meant to model Corporate ownership, Supply chain / Contract data and basic sourcing / provenance information.

## Massaging the data

Based on our decisions with the data model, we went back to the current data collected (in Google Spreadsheet) and made sure that it was properly structured to allow a painless import. We had already done some work to introduce data validation on Company names to make sure the import would go smoothly. We had to do more work to normalise dates and durations as well as prepare People data for the next version of the data model where people will be nodes, and deal with both Service Contracts and Production Contracts.

We then used the great Neo4j 2.1 LOAD CSV feature which is [a graph ETL "for everyone"](http://neo4j.com/blog/neo4j-2-1-graph-etl/). It allowed us to use code such as this to import a csv file (directly exported from the Google Spreadsheet):

```
// Load Nigeria - Contracts
LOAD CSV WITH HEADERS FROM 'https://raw.githubusercontent.com/iilab/openoil/master/data/Nigeria%20-%20Contracts%20-%20No%20Prod.csv' AS line
MERGE (source:Company { name: line.company_source } )
MERGE (target:Company { name: line.company_target } )
MERGE (contracttype:ContractType { name: line.type } )
CREATE (contract:Contract { name: line.name, official_title: line.offical_title, description:line.description, value_usd:line.value_usd, value_currency_amount:line.value_currency_amount, value_currency_unit: line.value_currency_unit, announcement_date: line.announcement_date, start_date:line.start_date, end_date: line.end_date, duration_months:line.duration_months, field:line.field, license_area:line.license_area})
CREATE (source)-[issuescontract:ISSUES {source_url: line.source_url, source_date: line.source_date, confidence: line.confidence, source_type:'' , log_message: line.log_message}]->(contract)
CREATE (contract)-[hascontractor:HAS_CONTRACTOR {contract_share:line.share, source_url: line.source_url, source_date: line.source_date, confidence: line.confidence, source_type:'' , log_message: line.log_message}]->(target)
CREATE (contract)-[hascontracttype:CONTRACT_TYPE {source_url: line.source_url, source_date: line.source_date, confidence: line.confidence, source_type:'' , log_message: line.log_message}]->(contracttype)
```

## Putting some Structr to it

While Structr reconsiders its adoption of neo4j 2.1, we couldn't directly import in Structr so we had to import in neo4j 2.1 and use neo4j-shell to transfer the data to structr (which can nicely coexist [on the same machine thanks to this fix](https://github.com/structr/structr/issues/159#issuecomment-44814248)). I use the develop branch of Structr using [these instructions](http://docs.structr.org/#Compile Source Code) with an added git checkout develop after cloning. 

To prevent the [stack overflow error with the default settings](https://github.com/structr/structr/issues/183#issuecomment-47385260) I had to increase the stack memory: 

 - Edit structr-ui/pom.xml to add the following line (around line 199 in the ```exec-maven-plugin``` arguments):
```
<argument>-Xss1g</argument>
```
 - Clean up the database (in structr-ui).
```
rm -rf db/*
```
 - Start Structr :
```
mvn exec:exex
```
 - Use neo4j-shell to dump the neo4j 2.1.1 (living on port 1337) data and pipe it to the Structr (living on port 1339):
```
neo4j-shell -port 1337  -c 'dump' | neo4j-shell -port 1339 -file -
```
Tada. 
```
Transaction started
+-------------------+
| No data returned. |
+-------------------+
Nodes created: 416
Relationships created: 795
Properties set: 6246
Labels added: 416
2632 ms
Transaction committed
```

No stack overflow error!

 - Now to the empty gist import ([here's an empty file for you](https://raw.githubusercontent.com/iilab/openoil/master/data/empty.gist)) bit to allow Structr to process the data schema, and tada da:

![image](https://cloud.githubusercontent.com/assets/356097/3420545/c0b584e8-feb0-11e3-9ef7-a40d801d3d2a.png)

 - With basic graph ready CRUD for free:

![image](https://cloud.githubusercontent.com/assets/356097/3420548/ecba2062-feb0-11e3-8cfa-f3953889ed57.png)

Amazing.

## Next

Next I'll have to import the rest of the data sets and start building the front end. On the road ahead I can already see some obstacles:

 - Relationship attributes are [not yet properly imported in the Schema](https://github.com/structr/structr/issues/179)
 - There are not yet [frontend elements in Structr to manipulate relationship properties](https://github.com/structr/structr/issues/179#issuecomment-47164571)
 - Building the visualisation layer needs a [more hands on review of the existing options that will easily rest on top of neo4j/structr and come with good feature sets](https://www.penflip.com/jun/iilab-graph/blob/master/technical-notes.txt#visualisation-layer) 
 - And of course, an order of magnitude more fun and ambitious, building an [user experience that actually allows non-tech people to investigate large graphs...](https://www.penflip.com/jun/iilab-graph/blob/master/graph-interfaces.txt)

More soon to come.
