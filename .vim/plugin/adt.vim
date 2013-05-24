"Author Chen Chi, call me Cook in English, and contact with me by
"reallychenchi@163.com or reallychenchi@gmail.com
"
"Functions:
"	AdtLogcat	Fetch logs from android device and display in copen
"			window. If there are error or warning stacks, it will
"			be recorgnized as error, so that you can switch between
"			error message and codes by :cn and :cp. By default, it
"			is mapped as "<Leader>l"
"
"	AdtBuild`	Build the current android application, make sure there
"			are build.xml and AndroidMenifest.xml in current folder.
"			The build log will be displayed in copen window, so
"			that you can switch between error message and codes by
"			:cn and :cp. By default, it is mapped as "<Leader>b"
"
"	AdtRun		Run the current android application, make sure there
"			are build.xml and AndroidMenifest.xml in current
"			folder. By default, it is mapped as "<Leader>r"
"
"	AdtClean	Clean the current android application build
"			envirovement. By default, it is mapped as "<Leader>c"
"
"	AdtHelp		Get the help information of the word under cursor,
"			the cooresponding html file in android docs folder
"			will be opened by xdg-open. By default, it is mapped
"			as "<Leader>h"
"
"	AdtJumpResouce	If the word under cursor is layout resource, it will
"			open the xml layout resource in a splitted window. By
"			default, it is mapped as "<Leader>j"

if exists("g:adtVimHtmlViewer") == 0
	let g:adtVimHtmlViewer = "xdg-open"
endif

if exists("g:adtVimAndroidPath") == 0
	let g:adtVimAndroidPath = ""
endif

" use "botright copen" to open below everything
if exists("g:adtVimCopenCmd") == 0
	let g:adtVimCopenCmd = "below copen"
endif

function! AdtJumpResource()
	let l:line = getline(line("."))
	let l:col = col(".")
	let l:lineH1 = strpart(l:line, 0, l:col)
	let l:lineH2 = strpart(l:line, l:col)
	let l:bns = split(bufname(winbufnr(winnr())), "\\.")
	if (len(l:bns) == 0)
		echo "File ".l:bn." is not supported, only *.java or *.xml support"
		return 1
	endif

	let l:bn = l:bns[-1]
	let l:resourceType = ""
	let l:resourceName = ""

	if l:bn == "java"
		let l:lineH1 = matchstr(l:lineH1, "\\v<R\\.([a-zA-Z_][a-zA-Z0-9_$\\.]*)$")
		let l:lineH2 = matchstr(l:lineH2, "\\v[a-zA-Z0-9_$\\.]*")
		let l:resourceName = l:lineH1.l:lineH2
		let l:resourceLines = split(l:resourceName, "\\.")
		if (len(l:resourceLines) != 3)
			echo l:resourceName." seems not an resource, while it should look like R.string.xxx, R.drawable.xxx and R.layout.xxx"
			return 1
		else
			let l:resourceType = l:resourceLines[1]
			let l:resourceName = l:resourceLines[2]
		endif
	elseif l:bn == "xml"
		let l:lineH1 = matchstr(l:lineH1, "\\v\\\"\\@([a-zA-z0-9\\_\\$\\/])*")
		let l:lineH2 = matchstr(l:lineH2, "\\v[a-zA-Z0-9\\_\\$]*\\\"")
		let l:resourceName = l:lineH1.l:lineH2
		let l:resourceType = matchstr(l:resourceName, "\\v(\\@)\@<=(\\a+)(\\/)\@=")
		let l:resourceName = matchstr(l:resourceName, "\\v(\\/)\@<=([a-zA-z0-9\\_\\$]+)")
	else
		echo "File ".l:bn." is not supported, only *.java or *.xml support"
		return 1
	endif

	let l:layoutDirs = GetTargetDir("res/")
	if l:resourceType == "layout"
		let l:fileName = l:resourceName.".xml"
		if JumpToFile(l:layoutDirs, l:fileName) == 0
			echo "Not found for ".l:fileName." in folders:".join(l:layoutDirs, ", ")
		endif
	elseif l:resourceType == "string"
		echo "This is a string resource named as ".l:resourceName.", please email to me what do you want it to do?"
	elseif l:resourceType == "drawable"
		let l:fileName = l:resourceName.".xml"
		if JumpToFile(l:layoutDirs, l:fileName) == 0
			echo "Not found for ".l:fileName." in folders:".join(l:layoutDirs, ", ")
		endif
	else
		echo l:resourceName. " is not resource of string, drawable nor layout"
	endif
endf

function! JumpToFile(layoutDirs, fileName)
	let l:ret = []
	for path in a:layoutDirs
		let path = path."**"
		let l:retTmp = findfile(a:fileName, path, -1)
		let l:ret = extend(l:ret, l:retTmp)
	endfor

	if len(l:ret) == 0
		return 0
	else
		let l:chooseLayout = GetInputLines(l:ret)
		if !empty(l:chooseLayout)
			exec "sp ".l:chooseLayout
		endif
		return 1
	endif
endf

function! AdtLogcat(errorFilter)
	exec "cclose"
	let l:devices = GetDevices()
	if len(l:devices) != 1
		echo "Failed to get log because only one device is supported, please check by command \"adb devices\""
		return 1
	endif

	let l:packageName = GetPackageName('./')
	if strlen(l:packageName) == 0
		echo "Failed to fetch the package name"
		return 1
	endif
	let l:pid = ""
	let l:psStr = system("adb shell ps")
	let l:psLines = split(l:psStr, "\n")
	let l:regexp = "\\v\\S+\\s+(\\d+\\s+){4}([0-9a-fA-f]+\\s+){2}\\S+\\s+\\M".l:packageName
	for line in l:psLines
		if match(line, l:regexp) > -1
			let l:pid = matchstr(l:line, "\\v(^\\S+\\s+)\@<=\\d+(\\s+\\d+)\@=")
		endif
	endfor

	let l:logstr = system("adb shell logcat -d")
	let l:logLines = split(logstr, "\n")
	if empty(l:pid)
		let l:regexp = "\\vActivityManager.*Start\\s*proc\\s*".l:packageName
		for line in l:logLines
			if match(line, l:regexp) > -1
				let l:pid = matchstr(line, "\\v\\s*pid\\s*\\=\\d+")
				let l:pid = matchstr(l:pid, "\\v\\d+")
			endif
		endfor
	endif

	if empty(l:pid)
		echo "Failed to fetch pid for ".l:packageName
		return 1
	endif

	let l:logAppLines = []
	for line in l:logLines
		if match(line, "\\v[".a:errorFilter."]\\/.{-}\\(\\s*".l:pid."\\s*\\)") > -1
			call add(l:logAppLines, line)
		endif
	endfor

	if len(l:logAppLines) == 0
		echo "No logs for ".l:packageName."(Pid=".l:pid.") found"
		return 1
	endif

	let l:sourceDir = GetTargetDir("src/")
	let l:regPre = "\\v[WE]\\/\\S{-}\\s*\\(\\s*".l:pid."\\s*\\)\\:\\s+at\\s*"
	let l:idx = 0
	for line in l:logAppLines
		let l:preLine = matchstr(line, l:regPre)
		if !empty(l:preLine)
			let l:infoLine = substitute(line, l:regPre, "", "")
			let l:packageInfo = matchstr(l:infoLine, "\\v\\S+(\\()\@=")
			let l:fileInfo = matchstr(l:infoLine, "\\v(\\()\@<=\\S+(\\:)\@=")
			let l:numInfo = matchstr(l:infoLine, "\\v(\\:)\@<=\\S+(\\))\@=")
			let l:fileName = GetFileName(l:sourceDir, l:packageInfo, l:fileInfo)
			if !empty(l:fileName)
				let line = "[E]".l:fileName.":".l:numInfo." ".l:preLine.l:packageInfo
				let l:logAppLines[l:idx] = line
			endif
		endif
		let l:idx = l:idx + 1
	endfor

	call writefile(l:logAppLines, "/tmp/l.txt")
	set efm=[E]%f:%l\ %m
	set makeprg=cat\ /tmp/l.txt
	exec "silent make"
	exec g:adtVimCopenCmd
	return 0
endf

function! AdtRun()
	exec "cclose"
	let l:devices = GetDevices()
	if len(l:devices) == 0
		echo "Installation canceld because only one device is supported, please check by command \"adb devices\""
		return 1
	endif

	echo "Installing..."
	let l:antRet = Ant("installd", 1)
	if match(l:antRet, "\\v^\\s+\\[exec\\]\\s+Failure\\s+\\[") != -1
		call writefile(l:antRet, "/tmp/l.txt")
		set makeprg=cat\ /tmp/l.txt
		exec "silent make"
		exec g:adtVimCopenCmd
		return 1
	else
		call AdtStart()
		return 0
	endif
endf

function! AdtStart()
	exec "cclose"
	let l:devices = GetDevices()
	if len(l:devices) == 0
		echo "Installation canceld because only one device is supported, please check by command \"adb devices\""
		return 1
	endif

	let l:packageName = GetPackageName('./')
	let l:mainActivity = GetMainActivity('./')
	let l:cmd = "adb shell am start -n ".l:packageName."/".l:mainActivity
	echo "Starting activity..."
	let l:execRet = system(l:cmd)
	echo l:execRet
	return 0
endf

function! AdtBuild()
	exec "cclose"
	echo "Building..."

	let l:antRet = Ant("debug", 0)
	if len(l:antRet) != 0
		let idx = 0
		let l:regAaptErr = "\\v\\s{-}\\[aapt\\]\\s(\\/\\S{-}){-}\\/\\S{-}\\:\\d+\\:\\serror\\:"
		for line in l:antRet
			if match(line, l:regAaptErr) > -1
				let line = substitute(line,"\\v\\s{-}\\[aapt\\]\\s", "    [javac]", "")
				let l:antRet[idx] = line
			endif
			let l:idx = l:idx + 1
		endfor
		call writefile(l:antRet, "/tmp/l.txt")
		set efm=%E\ \ \ \ [javac]%f:%l:\ %m,%C\ \ \ \ [javac]\ %m,%C\ \ \ \ [javac]\ %m
		set makeprg=cat\ /tmp/l.txt
		exec "silent make"
		exec g:adtVimCopenCmd
		return 1
	else
		echo "Build successful."
		call AdtRun()
		return 0
	endif
endf

function! AdtClean()
	exec "cclose"
	echo "Clean..."

	let l:antRet = Ant("clean", 0)
	if len(l:antRet) != 0
		echo "failed"
		call writefile(l:antRet, "/tmp/l.txt")
		set makeprg=cat\ /tmp/l.txt
		exec "silent make"
		exec g:adtVimCopenCmd
		return 1
	else
		echo "Clean successful."
		return 0
	endif
endf

function! AdtHelp()
	let l:line = getline(line("."))
	let l:col = col(".")
	let l:lineH1 = strpart(l:line, 0, l:col)
	let l:lineH1 = matchstr(l:lineH1, "\\v<[a-zA-Z_][a-zA-Z0-9_$]*$")
	let l:lineH2 = strpart(l:line, l:col)
	let l:lineH2 = matchstr(l:lineH2, "\\v[a-zA-Z0-9_$]*")
	let l:word = l:lineH1.l:lineH2
	"let l:tags = taglist(l:word)
	if empty(g:adtVimAndroidPath)
		let l:path = system("which android")
	else
		let l:path = g:adtVimAndroidPath
	endif

	if empty(l:path)
		echo "Android is not installed, documents not found package"
	else
		echo "Searching..."
		let l:fileList = []
		let l:path = matchstr(l:path, "\\v\\S+(\\/tools\\/android)\@=")
		let l:classes = l:path . "/docs/reference/classes.html"
		let l:lines = readfile(l:classes, '')
		let l:searchReg = "\\v\\<a\\s+href\\=\\\".*\\\"\\>(.*\\.)*<".l:word.">(\\..*)*\\<\\/a\\>"
		for line in l:lines
			let l:linkName = matchstr(line, l:searchReg)
			if (!empty(l:linkName))
				let l:link = matchstr(l:linkName, "\\v(\\<a\\s+href\\=\\\")\@<=(.{-})(\\\"\\>.{-}\\<\\/a\\>)\@=")
				let l:name = matchstr(l:linkName, "\\v(\\<a\\s+href\\=\\\".{-}\\\"\\>)\@<=(.{-})(\\<\\/a\\>)\@=")
				let l:link = l:path."/docs/reference/".l:link
				call add(l:fileList, l:link)
			endif
		endfor

		"let l:finds = system(l:cmd)
		"let l:fileList = split(l:finds, \"\n\")
		if len(l:fileList) > 0
			let l:fn = GetInputLines(l:fileList)
			if !empty(l:fn)
				exec "!".g:adtVimHtmlViewer." ".l:fn
				return 0
			endif
		else
			echo "Keyword ".l:word." is not found in android documents"
		endif
	endif

	return 1
endf

function! GetInputLines(chooseList)
	if len(a:chooseList) == 1
		return a:chooseList[0]
	else
		let l:displayInput = []
		let l:idx = 0
		for line in a:chooseList
			let l:displayLine = l:idx.":\t".line
			call add(l:displayInput, l:displayLine)
			let l:idx = l:idx + 1
		endfor
		let l:idx = inputlist(l:displayInput)
		if (l:idx > -1 && l:idx < len(a:chooseList))
			return a:chooseList[l:idx]
		else
			return ""
		endif
	endif
endf

function! Ant(para, noCheck)
	let l:antRet = system("ant ".a:para)
	if a:noCheck == 1
		return split(l:antRet, "\n")
	endif

	let l:antSuccessReg = "\\vBUILD\\s+SUCCESSFUL.*Total\\s+time\\:\\s+\\d+\\s+second"
	let l:antRunRet = matchstr(l:antRet, l:antSuccessReg)
	if empty(l:antRunRet)
		return split(l:antRet, "\n")
	else
		return []
	endif
endf

function! GetFileName(dirs, packageName, fn)
	let l:ret = ""

	let l:fileName = matchstr(a:fn, "\\v\\S+(\\.)\@=")
	let l:dirName = matchstr(a:packageName, "\\v\\S+(".l:fileName.")\@=")
	let l:fileName = substitute(l:dirName, "\\.", "/", "g").a:fn
	for dir in a:dirs
		let l:ret = dir.l:fileName
		let l:ret = findfile(l:ret, ".;")
		if !empty(l:ret)
			break
		endif
	endfor
	return l:ret
endf

function! GetTargetDir(target)
	let l:ret = ["./".a:target]
	let l:fn = "project.properties"
	let l:regAndroidLibPrj = "\\v(android\\.library\\.reference\\.\\d+\\s*\\=)@<=\\S+($)\@="
	let l:lines = readfile(l:fn, '')
	for line in l:lines
		let l:libSource = matchstr(line, l:regAndroidLibPrj)
		if !empty(l:libSource)
			call add(l:ret, l:libSource."/".a:target)
		endif
	endfor

	return l:ret
endf

function! GetMainActivity(path)
	let l:ret = ""
	let l:fn = a:path . 'AndroidManifest.xml'
	let l:str = GetFileContent(l:fn)
	let l:nodes = GetNodes(l:str, 'manifest', 'application', 'activity')
	for node in l:nodes
		let l:activityName = GetProperty(node, 'android:name')
		let l:actions = GetNodes(node, 'intent-filter', 'action')
		for action in l:actions
			let l:actName = GetProperty(action, 'android:name')
			if l:actName == "android.intent.action.MAIN"
				if match(l:activityName, "\\.") == -1
					let l:ret = ".".l:activityName
				else
					let l:ret = l:activityName
				endif
				break
			endif
		endfor
		if !empty(l:ret)
			break
		endif
	endfor
	return l:ret
endf

"The get package name from path
function! GetPackageName(path)
	let l:ret = ""
	let l:fn = a:path . 'AndroidManifest.xml'
	let l:str = GetFileContent(l:fn)
	let l:nodes = GetNodes(l:str, 'manifest')
	for node in l:nodes
		let l:ret = GetProperty(node, 'package')
		if !empty(l:ret)
			break
		endif
	endfor

	return l:ret
endf

function! GetFileContent(fn)
	let l:lines = readfile(a:fn, '')
	let l:str = ''
	for line in lines
		let l:str = l:str . line
	endfor
	return l:str
endf

function! GetNodes(str, ...)
	let l:ret = []
	let l:num = a:0
	let l:lastNode = a:000[num - 1]
	let l:names = []
	let l:curNodes = [a:str]
	let l:curBNodes = []
	for index in range(len(a:000) - 1)
		call add(l:names, a:000[index])
	endfor

	for name in l:names
		for node in l:curNodes
			let l:nodes = GetMatchList(node, "\\v\\<".name.".{-}\\>.{-}\\<\\/".name."\\>")
			for nodeTiny in l:nodes
				call add(l:curBNodes, nodeTiny)
			endfor
		endfor
		let l:curNodes = l:curBNodes
		let l:curBNodes = []
	endfor

	let l:ret = []
	for node in curNodes
		let l:lastNodes = GetMatchList(node, "\\v(\\<".l:lastNode.".{-}\\>.{-}\\<\\/".l:lastNode."\\>)|".
					\"(\\<".l:lastNode.".{-}\\>.{-}\\/\\>)")
		for retNode in l:lastNodes
			call add(l:ret, retNode)
		endfor
	endfor

	return l:ret
endf

function! GetMatchList(str, pattern)
	let l:ret = []
	let l:pos = 0
	let l:len = strlen(a:str)

	while l:pos < l:len
		let l:str = matchstr(a:str, a:pattern, l:pos)
		if empty(l:str)
			break
		endif
		let l:pos = l:pos + strlen(l:str)
		call add(l:ret, l:str)
	endwhile
	return l:ret
endf

function! GetProperty(str, property)
	let l:pro = matchstr(a:str, "\\v".a:property."\\s*\\=\\s*\\\".{-}\"")
	let l:pro = matchstr(l:pro, "\\v\\\".*\"")
	let l:ret = l:pro[1:-2]
	return l:ret
endf

function! GetDevices()
	let l:ret = []
	let l:checkRets = system("adb devices")
	let l:checkRet = split(l:checkRets, "\n")
	let l:idx = -1
	while l:idx < len(l:checkRet)
		let l:line = l:checkRet[l:idx]
		let l:idx = l:idx + 1
		if match(l:line, "List of devices attached") > -1
			break
		endif
	endwhile
	while l:idx < len(l:checkRet)
		let l:line = l:checkRet[l:idx]
		let l:idx = l:idx + 1
		if match(l:line, "\\v\\sdevice") > 0
			let l:deviceName = matchstr(l:line, "\\v\\S+")
			if match(l:deviceName, "\\v^\\?+$") == -1
				call add(l:ret, l:deviceName)
			endif
		endif
	endwhile

	return l:ret
endf

nmap <Leader>e :call AdtLogcat("E")<cr>
nmap <Leader>l :call AdtLogcat("WDIE")<cr>
nmap <Leader>b :call AdtBuild()<cr>
nmap <Leader>c :call AdtClean()<cr>
nmap <Leader>r :call AdtRun()<cr>
nmap <Leader>s :call AdtStart()<cr>
nmap <Leader>h :call AdtHelp()<cr>
nmap <Leader>j :call AdtJumpResource()<cr>
