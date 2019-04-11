# DMC2019
## Workflow

[A Quick Tutorial](https://www.liaoxuefeng.com/wiki/0013739516305929606dd18361248578c67b8067c8c017b000)

### Before You Start

1. Clone the Repo: git clone git@github.com:tianqinglong/dmc2019.git

    or you could try:   git clone https://github.com/tianqinglong/dmc2019.git

2. Now you will only see the master branch. So you will need to create the local "dev" branch by: 

   - git checkout -b dev origin/dev

3. Other people may have pushed commits to "dev". Before you push your commit, you should update your local "dev" by:

   - git branch --set-upstream-to=origin/dev dev

   - git pull

4. Create your own branch locally:

   - git checkout -b YOUR_BRANCH_NAME

5. Now you are able to work on your own branch. When you are done merge your own branch with dev and then push to the remote (May need step 3).

   - (suppose you are in your own branch "YOUR_BRANCH_NAME") git checkout dev
   - git pull
   - git merge YOUR_BRANCH_NAME

6. Never work on "master" branch. 
