    bh-ip query -p sw-1/2
    bh-ip query -m 1:2:3:4:5:6
    bh-ip query -i 1.2.3.4

these are just queries.

    bh-ip queryhost -p sw-1/2 bh123
    bh-ip queryhost -m 1:2:3:4:5:6 bh123
    bh-ip queryhost -i 1.2.3.4 bh123

these are the same as queries above, but all retrieved information (port, mac
and ip) will be added to inventory as belonging to host 'bh123'. First, the
inventory will be inspected and then remote queries verifying found info would
be done.  If info is incorrect, full search will begin.

    bh-ip queryhost --cache ...

will just query inventory without doing any remote queries.

    bh-ip export inventory.csv

will export inventory to csv.

    bh-ip import inventory.csv

will import inventory from csv.

    bh-ip update

will verify and update info about all hosts found in inventory.
