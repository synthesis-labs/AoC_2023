package helper

import (
	"bufio"
	"fmt"
	"net/http"

	"github.com/spf13/viper"
)

func ViperEnvVariable(key string) string {

	viper.SetConfigFile("../../.env")

	err := viper.ReadInConfig()

	if err != nil {
		panic(err)
	}

	value, ok := viper.Get(key).(string)
	if !ok {

		panic("Invalid type assertion")
	}

	return value
}

func GetFileContents(year int, day int) []string {

	var output []string

	client := http.Client{}
	req, err := http.NewRequest("GET", fmt.Sprintf("https://adventofcode.com/%d/day/%d/input", year, day), nil)

	if err != nil {
		panic(err)
	}

	session_env := ViperEnvVariable("SESSION")

	cookieSession := http.Cookie{Name: "session", Value: session_env}
	req.AddCookie(&cookieSession)

	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	scanner := bufio.NewScanner(resp.Body)
	for i := 0; scanner.Scan(); i++ {
		var line = scanner.Text()
		output = append(output, line)
	}

	return output
}
