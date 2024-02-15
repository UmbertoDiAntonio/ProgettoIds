package ids.unicam.models.contenuti;

import ids.unicam.Comune;
import ids.unicam.models.Tempo;
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

    @ElementCollection(fetch = FetchType.EAGER)
    private List<String> tags = new ArrayList<>();

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


    public List<String> getTags() {
        return tags;
    }

    public ContenutoGenerico(Comune comune) {
        this.comune=comune;
    }
    public ContenutoGenerico(){}

    public Comune getComune() {
        return comune;
    }
}
