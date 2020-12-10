---
title: Using Hakyll with GitHub Pages has become even easier!
author: Thomas Mahler
tags: Hakyll, GitHub Pages
---

Over the weekend I've set up a [hakyll](https://jaspervdj.be/hakyll/) powered blog on github pages (actually the one you are reading right now).

I'd like to share my findings, as I found an easier way to integrate Hakyll with GitHub Pages.

## The documented way

I followed the tutorial [Using Hakyll with GitHub Pages](https://jaspervdj.be/hakyll/tutorials/github-pages-tutorial.html).
This tutorial assumes that the GitHub pages must always be served from the root folder of a github repository. 

It then describes a way to achieve this by using a `develop` branch to do all the Hakyll work and finally writing the contents to the default `_site` folder.
This folder is excluded from version control by entries in the `.gitignore` file both in the `develop` and `master` branches.

So to finally publish the generated site you'll have to switch to the `master` branch and copy the contents of the `_site` folder to the root folder of your project.

I tried this approach and it works nicely.

But then I found out that GitHub pages also allows to use a `docs` folder as the document root of your GitHub Pages site.

This makes things significantly easier, as you can do all the necessary hakyll tasks and the final publishing on the same `master` branch.

## The easier way

### GitHub setup

1. If required, create a new GitHub repository for your blog.
2. If required, create a master branch.
3. in the Settings of your GitHub project define that the `/docs` folder from the `master` branch should be used as document-root of your site.
   Please refer to the [documentation](https://docs.github.com/en/free-pro-team@latest/github/working-with-github-pages/configuring-a-publishing-source-for-your-github-pages-site#choosing-a-publishing-source)
   in case of problems.
4. Create a .gitignore file with at a minimum, the following entries:

```bash
_cache/
.stack-work/
```

### Local setup
If required, create a new Hakyll project. If you’re a stack user, there is a Hakyll template available that makes this step easy:

```bash
stack new myblog hakyll-template
```

Create a .gitignore file in your blog’s directory with at a minimum, the same directories listed as in the GitHub repository.
Use the following git commands to setup your local repository:

```bash
git init
# track all the source files for our blog.
git add .
# make our first commit
git commit -m "initial commit."
# and add the GitHub repository as a remote.
git remote add origin <URL to your GitHub pages repository>
```

### Modify site.hs

In order to make Hakyll generate the site into the `docs` folder we have to edit the Hakyll Main module (`site.hs` if you use the stack template) slightly:

```haskell
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

main :: IO ()
main = do
  hakyllWith config $ do
  ...
```

### Deployment

So everything’s all setup, and we’re ready to deploy.

We need to be able to run the executable that generates the website, so we need to compile it first. If you are using stack, this can be done using:

```bash
stack build
```

Next we get a clean build of our site:

```bash
stack exec myblog clean
stack exec myblog build
```

After this step you should see a folder `docs` under your projects root folder, which contains the generated Hakyll site.

Now we commit our changes:

```bash
git add -A
git commit -m "Publish."
```

And send them to GitHub:

```bash
git push origin master:master
```

That's all.

Now your Hakyll site should be visible under your GitHub Pages URL!

## View the source

The source code for this blog post lies under my [thma.github.io](https://github.com/thma/thma.github.io) GitHub project.
I've adopted the composeconference css from [Katy Chuangs great Hakyll CSS Garden](http://katychuang.com/hakyll-cssgarden/gallery/) 
and tweaked it a little bit to look more like GitHub markdown style and to provide some basic responsive design.

## Update Nov. 24th 2020

The Hakyll project has just accepted my pull request for the Using Hakyll with GitHub Pages
tutorial, which updates the text according to the findings presented in this post.

The amended version will thus be online shortly, rendering this blog entry kind of obsolete...
