package ids.unicam.models.contenuti;

import ids.unicam.Comune;
import ids.unicam.utilites.Stato;
import jakarta.persistence.*;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;


@MappedSuperclass
public abstract class ContenutoGenerico {

    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE,generator = "sequenza_contenuti")
    @SequenceGenerator(name = "sequenza_contenuti", sequenceName = "PUNTI_DI_INTERESSE_SEQ", allocationSize = 1)
    private int id= 0;

    @OneToOne
    @JoinColumn(name = "nome_comune")
    private Comune comune;
    private Stato stato = Stato.NOT_APPROVED;

    @OneToMany(fetch = FetchType.EAGER)
    private List<Tag> tags = new ArrayList<>();

    private LocalDate expireDate = LocalDate.MAX;
    public boolean isExpired() {
        return LocalDate.now().isAfter(expireDate);
    }

    public void setExpireDate(LocalDate expireDate) {
        this.expireDate = expireDate;
    }

    public LocalDate getExpireDate() {
        return expireDate;
    }

    public int getId() {
        return id;
    }

    public Stato getStato() {
        return stato;
    }

    public void setStato(Stato approved) {
        this.stato = approved;
    }


    public List<Tag> getTags() {
        return tags;
    }
    public void resetTags(){
        this.tags=new ArrayList<>();
    }

    public ContenutoGenerico(Comune comune) {
        this.comune=comune;
    }
    public ContenutoGenerico(){}

    public Comune getComune() {
        return comune;
    }
}
