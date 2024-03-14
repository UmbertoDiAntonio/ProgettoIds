<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Progetto "Ingegneria del Software A.A. 2023/2024"</title>
</head>
<body>
    <h1>Progetto "Ingegneria del Software A.A. 2023/2024"</h1>
    <h2>Descrizione del Progetto</h2>
    <p>Il progetto "Ingegneria del Software A.A. 2023/2024" si propone di sviluppare una piattaforma per la valorizzazione e digitalizzazione di un territorio comunale. La piattaforma permetterà il caricamento di informazioni di vario tipo, tra cui contenuti culturali, turistici, sportivi e altro, al fine di interessare sia i residenti che i visitatori del territorio.</p>
    <h2>Installazione e Utilizzo</h2>
    <ol>
        <li>Clona il repository.</li>
        <li>Esegui <code>mvn clean install</code> per installare le dipendenze.</li>
        <li>Avvia il server con <code>java -jar [percorso file.jar]</code>.</li>
    </ol>
    <p>Una volta eseguito il terzo comando, l'applicazione si avvierà automaticamente in due schermate: una dedicata alla piattaforma con l'interfaccia grafica Swagger e una dedicata al database H2.</p>
    <p>Per accedere al database H2 e visualizzare i dati, utilizza le seguenti credenziali:</p>
    <ul>
        <li><strong>Username</strong>: <code>user</code></li>
        <li><strong>Password</strong>: <code>pass</code></li>
    </ul>
    <p>Nell'interfaccia Swagger è possibile effettuare chiamate HTTP con i metodi messi a disposizione.</p>
    <p>Il gestore del database viene creato in automatico e avrà le seguenti credenziali:</p>
    <ul>
        <li><strong>Username</strong>: <code>admin</code></li>
        <li><strong>Password</strong>: <code>admin</code></li>
    </ul>
    <p>L'username sarà necessario per richiamare i metodi del gestore della piattaforma. Non sono messi a disposizione metodi per la modifica delle credenziali del gestore della piattaforma.</p>
    <h2>Studenti per lo sviluppo del progetto</h2>
   <table>
    <thead>
        <tr>
            <th>Nome</th>
            <th>Cognome</th>
            <th>Matricola</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>Umberto</td>
            <td>Di Antonio</td>
            <td>120024</td>
        </tr>
        <tr>
            <td>Leonardo</td>
            <td>Compagnucci</td>
            <td>118708</td>
        </tr>
        <tr>
            <td>Riccardo</td>
            <td>Marini</td>
            <td>345678</td>
        </tr>
    </tbody>
</table>
</body>
</html>
