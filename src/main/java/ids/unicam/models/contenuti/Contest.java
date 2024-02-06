package ids.unicam.models.contenuti;

import ids.unicam.controller.ContestController;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.Animatore;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.utilites.Stato;
import jakarta.persistence.Entity;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Transient;

import java.util.ArrayList;
import java.util.List;

@Entity
public class Contest extends ContenutoGenerico {
    private boolean open;
    @Transient
    private ContestController controller=null;
    private String obiettivo="";
    @OneToOne
    private Animatore creatore =null;
    @OneToMany
    private final ArrayList<TuristaAutenticato> partecipanti = new ArrayList<>();
    @OneToMany
    private final ArrayList<Invito> inviti = new ArrayList<>();
    @OneToMany
    private final ArrayList<MaterialeGenerico> materiali = new ArrayList<>();
    private String nome=null;

    public Contest() {

    }

    
    public Animatore getCreatore() {
        return creatore;
    }

    public String getNome() {
        return nome;
    }


    public ArrayList<Invito> getInviti() {
        return inviti;
    }
    public void setOpen(boolean open) {
        this.open = open;
    }

    public String getObiettivo() {
        return obiettivo;
    }


    public ArrayList<MaterialeGenerico> getMateriali() {
        return materiali;
    }

    public boolean isOpen() {
        return open;
    }

    
    public List<TuristaAutenticato> getPartecipanti() {
        return partecipanti;
    }


    
    public ContestController getContestController() {
        return controller;
    }



    public Contest(String nome, boolean open, ContestController controller, String obiettivo, Animatore creatore) {
        super();
        setStato(Stato.APPROVED);
        this.open = open;
        this.controller = controller;
        this.obiettivo = obiettivo;
        this.creatore = creatore;
        this.nome = nome;
    }



}
