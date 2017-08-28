studio
======

studio is a simple static site generator for a web journal. Making it easy to convert your text articles in to webpages.

studio organizes the html pages and generates a table of contents. Having the flexibility to change a template and adding custom css and javascript. With static webpages, your site is fast, secure and easy to deploy 

Installing
----------

studio requires **ghc** for compiling haskell and **pandoc** for managing templating 


Setup
-------

	/Articles
		/Article 1
			words.md
		/Article 2
			words.md
	/Output -- the output website
	template.html


### structure words.md

everything is written in markdown. To help place the in the system.

	% Habits
	% John Doe
	% March 22, 2005

Add the Title, Author and Date. This will places the dates in the order they were made
