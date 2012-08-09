# QuickSpeak

QuickSpeak est un client IM qui fonctionne avec un fichier pour
stocker les données. Il a été conçu pour fonctionner en utilisant un
fichier dans un dossier partagé Dropbox® mais il devrait fonctionner
également avec n'importe quel fichier.

Le principe de fonctionnement est simple : L'application surveille les
modifications du fichier choisi et affiche le contenu du fichier
chaque fois qu'il est modifié.

Il est possible d'écrire dans ce fichier depuis le même terminal.

## Installation & Utilisation

    git clone https://github.com/niahoo/quickspeak.git
    cd quickspeak
    rebar compile
    bin/start myLogin /path/to/my/file
