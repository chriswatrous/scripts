package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"os/user"
	"path"
	"strconv"
	"strings"
)

const DIR_COLOR = "blue"
const PROMPT_COLOR = "cyan"
const COMMAND_COLOR = "yellow"
const CLEAN_TREE_COLOR = "green"
const DIRTY_TREE_COLOR = "red"
const ROOT_WARNING_COLOR = "red"

func main() {
	initColorCodes()

	statusParts := make([]string, 0, 10)

	// Add root warning.
	if os.Geteuid() == 0 {
		statusParts = append(statusParts,
			colorize(ROOT_WARNING_COLOR, "(root)"))
	}

	// Add working dir.
	statusParts = append(statusParts, colorize(DIR_COLOR, getwd()))

	// Add git branch.
	branch := gitBranch()
	if branch != "" {
		statusParts = append(statusParts, branch)
	}

	fmt.Print("\n[", strings.Join(statusParts, " "), "]\n")
}

func isTreeClean() (out bool) {
	cmd := exec.Command("git", "status", "-s")
	var outbuf, errbuf bytes.Buffer
	cmd.Stdout = &outbuf
	cmd.Stderr = &errbuf
	err := cmd.Run()
	check(err)
	return strings.TrimSpace(outbuf.String() + errbuf.String()) == ""
}

func gitBranch() (out string) {
	repoDir := gitRepoDir()
	if repoDir == "" {
		return ""
	}

	head, err := ioutil.ReadFile(path.Join(repoDir, ".git/HEAD"))
	check(err)
	branch := strings.TrimSpace(string(head))
	if strings.HasPrefix(branch, "ref: refs/heads/") {
		branch = strings.TrimPrefix(branch, "ref: refs/heads/")
	}
	var color string
	if isTreeClean() {
		color = CLEAN_TREE_COLOR
	} else {
		color = DIRTY_TREE_COLOR
	}
	return colorize(color, branch)
}

func gitRepoDir() (out string) {
	oldRepoDir := ""
	repoDir := getwd()

	for repoDir != oldRepoDir {
		if isGitRepo(repoDir) {
			return repoDir
		}
		oldRepoDir = repoDir
		repoDir, _ = path.Split(repoDir)
	}

	return ""
}

func isGitRepo(dir string) (out bool) {
	_, err := os.Stat(path.Join(dir + "/.git"))
	return err == nil
}

func getwd() (out string) {
	wd, err := os.Getwd()
	check(err)
	return wd
}

func replaceHome(dir string) (out string) {
	usr, err := user.Current()
	check(err)
	fmt.Println(usr.HomeDir)
	if strings.HasPrefix(dir, usr.HomeDir) {
		return "~" + strings.TrimPrefix(dir, usr.HomeDir)
	} else {
		return dir
	}
}

func colorize(colorSpec, s string) (out string) {
	return colorSequence(colorSpec) + s + colorSequence("off")
}

func colorSequence(colorSpec string) (out string) {
	colors := strings.Split(colorSpec, ",")
	codeStrs := make([]string, len(colors))
	for i := 0; i < len(colors); i++ {
		codeStrs[i] = strconv.Itoa(colorCodes[colors[i]])
	}
	return fmt.Sprintf("\x1b[%vm", strings.Join(codeStrs, ";"))
}

var colorCodes map[string]int

func initColorCodes() {
	colorCodes = map[string]int{
		"off":       0,
		"bold":      1,
		"dim":       2,
		"underline": 4,
		"reverse":   7,
		"concealed": 8,

		"black":   30,
		"red":     31,
		"green":   32,
		"yellow":  33,
		"blue":    34,
		"magenta": 35,
		"cyan":    36,
		"white":   37,
		"gray":    90,

		"bk_black":   40,
		"bk_red":     41,
		"bk_green":   42,
		"bk_yellow":  43,
		"bk_blue":    44,
		"bk_magenta": 45,
		"bk_cyan":    46,
		"bk_white":   47,
		"bk_gray":    100,

		// These don't work in emacs.
		"light_red":     91,
		"light_green":   92,
		"light_yellow":  93,
		"light_blue":    94,
		"light_magenta": 95,
		"light_cyan":    96,
		"light_white":   97,

		"bk_light_red":     101,
		"bk_light_green":   102,
		"bk_light_yellow":  103,
		"bk_light_blue":    104,
		"bk_light_magenta": 105,
		"bk_light_cyan":    106,
		"bk_light_white":   107,
	}
}

func check(e error) {
	if e != nil {
		panic(e)
	}
}
