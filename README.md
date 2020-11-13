# File Manager + VCS
## Available commands:
* `cd <folder>` – change the directory
* `ls <folder>` – list information about files in the current directory
* `mkdir "folder-name"` – create directory in the current directory
* `cat <file>` – show file content
* `mkfile "file-name"` – create file in the current directory
* `remove <file>` – remove file
* `write-file <file> "text"` – write text to the file
* `find-file "file-name"` – find file in the current dir and its subdirs
* `information <file | folder>` – show information about the file or folder
* `cvs-init` – init VCS in the current directory
* `cvs-add <file | folder>` – add the file or folder to the VCS 
* `cvs-update <file> "comment"` –  commit changes
* `cvs-history <file>` – show history of the file
* `cvs-cat <file> "index"` – show version of the file
* `merge <file> "index1" "index2" "left | right | both | interactive"` – merge version of the file (need fix)
* `cvs-delete-version <file> "index"` – delete version of the file
* `cvs-remove <file>` – delete file from the VCS
* `help`
* `exit`

## P.S.
The project is here not because it's ready for use! It's just an introductory homework which took too much of my effort and became surprisingly nice (don't even try to prove to me that it is not!) :)
