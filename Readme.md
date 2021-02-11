Generates invoices (as a PDF) from timesheets in a CSV format.  This has been tested against [harvest](http://www.getharvest.com) but can accept other formats assuming they have the correct headers (Date, Client, Project, Task, Notes, Hours).  

To use it, make a copy of the sample data directory (naming it just `data`) and then fill in personal and client info in the relevant JSON files.  You can place timesheets (CSV) files in the `timesheets` subdirectory of `data` but it's not a requirement.  Command to generate the invoice (PDF) is (requires [cabal](https://www.haskell.org/cabal/)):

    cabal v2-run haskell-pdf -- --client-name=SampleClientLLC --timesheet-file=data/SampleClientLLC/timesheets/october-2021.csv --rate=50

Run `cabal v2-run haskell-pdf -- --help` for more command options.

See the [sample invoice](sample.pdf) to see what the generated invoice looks like.

