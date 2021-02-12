Generates invoices (as a PDF) from timesheets in a CSV format.  This has been tested against [harvest](http://www.getharvest.com) but can accept other formats assuming they have the correct headers (Date, Client, Project, Task, Notes, Hours).  

To use it, make a copy of the `data.sample` directory (naming it just `data`) and then fill in personal and client info in the relevant JSON files.  If no timesheet file path is provided, it will pick from the latest in the `data/<client_name>/timesheets` directory.  Command to generate the invoice (PDF) is (requires [cabal](https://www.haskell.org/cabal/)):

    cabal v2-run haskell-pdf -- --client-name=SampleClientLLC --rate=50

Run `cabal v2-run haskell-pdf -- --help` for more command options.

See the [sample invoice](sample.pdf) to see what the generated invoice looks like.

