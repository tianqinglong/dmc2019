# DMC2019
## Workflow
### Before You Start
1. Clone the Repo: git clone git@github.com:tianqinglong/dmc2019.git
2. Now you will only see the master branch. So you will need to create the local "dev" branch by: git checkout -b dev origin/dev
3. Now you can work on the "dev" branch.
4. Other people may have pushed commits to "dev". You may need:
- git branch --set-upstream-to=origin/dev dev
- git pull
to merge the remote "dev" with your local "dev".
5. You may want to work on your own branch. When you are done, merge your branch to "dev".
