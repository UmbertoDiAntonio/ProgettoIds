package ids.unicam;

import ids.unicam.OSM.RichiestaOSM;
import ids.unicam.controller.ContenutoController;
import ids.unicam.controller.ContestController;
import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.attori.GestorePiattaforma;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Punto;
import jakarta.persistence.*;


@Entity
@Table(name = "COMUNI")
public class Comune {
    @Id
    @Column(name = "comune")
    private String nome = "";
    @Embedded
    private Punto posizione;

       @Transient
    private final ContestController contestController = new ContestController();
    @Transient
    private GestorePiattaforma gestorePiattaforma = null;

    public Comune() {

    }


    public GestorePiattaforma getGestorePiattaforma() {
        return gestorePiattaforma;
    }


    public Punto getPosizione() {
        return posizione;
    }




    public ContestController getContestController() {
        return contestController;
    }

    public String getNome() {
        return nome;
    }

    /**
     * @param nome               Nome del Comune
     * @param gestorePiattaforma il gestore della piattaforma
     * @throws IllegalArgumentException se il nome del comune non corrisponde a nessun comune
     * @throws RuntimeException         se non è possibile raggiungere il sistema OSM
     */
    public Comune(String nome, GestorePiattaforma gestorePiattaforma) {

        //      ComuneController.getInstance().listaComuni.add(this);
        this.gestorePiattaforma = gestorePiattaforma;
        try {
            this.posizione = RichiestaOSM.getCoordinateDaComune(nome);
            this.nome = RichiestaOSM.getComuneDaCoordinate(posizione);

            if (this.nome == null)
                throw new RuntimeException("Impossibile stabilire la connessione con il sistema OSM");

            if (!this.nome.equalsIgnoreCase(nome))
                throw new IllegalArgumentException("Il nome del comune ricercato non corrisponde con nessun comune reale");

        } catch (ConnessioneFallitaException e) {
            Main.logger.error("Connessione fallita durante il recupero delle coordinate o del nome del comune da OSM", e);
        }
    }


    public final boolean verificaCoordinateComune(Punto punto) {
        String nomeComune = RichiestaOSM.getComuneDaCoordinate(punto);
        if (nomeComune != null) {
            return nomeComune.equalsIgnoreCase(getNome());
        }
        return false;
    }
}
