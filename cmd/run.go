package cmd

import (
	"fmt"
	"io/ioutil"
	"os"

	"github.com/bmatsuo/elps/lisp"
	"github.com/bmatsuo/elps/parser"
	"github.com/spf13/cobra"
)

var (
	runExpression bool
	runPrint      bool
)

// runCmd represents the run command
var runCmd = &cobra.Command{
	Use:   "run",
	Short: "Run lisp code",
	Long:  `Run lisp code provided supplied via the command line or a file.`,
	Run: func(cmd *cobra.Command, args []string) {
		exprs, err := runReadExpressions(args)
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}

		env := lisp.NewEnv(nil)
		for i := range exprs {
			complete, err := parser.Parse(env, runPrint, exprs[i])
			if err != nil {
				fmt.Fprintln(os.Stderr, err)
				os.Exit(1)
			}
			if !complete {
				fmt.Fprintln(os.Stderr, "syntax error")
				os.Exit(1)
			}
		}
	},
}

func runReadExpressions(args []string) ([][]byte, error) {
	exprs := make([][]byte, len(args))
	if runExpression {
		for i := range args {
			exprs[i] = []byte(args[i])
		}
		return exprs, nil
	}
	for i, path := range args {
		b, err := ioutil.ReadFile(path)
		if err != nil {
			return nil, err
		}
		exprs[i] = b
	}
	return exprs, nil
}

func init() {
	rootCmd.AddCommand(runCmd)

	// Here flags for the run command are defined
	runCmd.Flags().BoolVarP(&runExpression, "expression", "e", false,
		"Interpret arguments as lisp expressions")
	runCmd.Flags().BoolVarP(&runPrint, "print", "p", false,
		"Print expression values to stdout")
}
