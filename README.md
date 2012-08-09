# QuickSpeak

QuickSpeak est un client IM qui fonctionne avec un fichier pour
lire et écrire les messages. Il a été conçu pour fonctionner en 
utilisant un fichier dans un dossier partagé 
[Dropbox](https://www.dropbox.com/) mais il devrait fonctionner 
également avec n'importe quel fichier.

Le principe de fonctionnement est simple : L'application surveille les
modifications du fichier choisi et affiche le contenu du fichier
chaque fois qu'il est modifié.

Il est possible d'écrire dans ce fichier depuis le même terminal.

## Installation & Utilisation

### Clonage du dépôt et compilation

    git clone https://github.com/niahoo/quickspeak.git
    cd quickspeak
    make
    
### Lancement du client

Lancer la commande suivante:
    
    bin/start Nickname /path/to/filename

* Nickname est votre pseudo, il ne doit pas contenir d'espace.
* Le fichier indiqué sera créé s'il n'existe pas.

### Fermeture du client

Taper Ctrl+C deux fois.
