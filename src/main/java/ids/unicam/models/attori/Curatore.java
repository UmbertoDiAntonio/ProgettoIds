package ids.unicam.models.attori;

import ids.unicam.models.contenuti.MaterialeGenerico;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.utilites.Observer;
import ids.unicam.utilites.Stato;
import jakarta.persistence.*;

import java.util.ArrayList;


@Entity
@Table(name = "CURATORI")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class Curatore extends ContributorAutorizzato {
    public Curatore() {
    }

    public Curatore(Contributor contributor) {
        super(contributor);
    }

    @Transient
    private final ArrayList<Observer> osservatori = new ArrayList<>();

    public void aggiungiOsservatore(Observer osservatore) {
        osservatori.add(osservatore);
    }

    public void rimuoviOsservatore(Observer osservatore) {
        osservatori.remove(osservatore);
    }

    public void notifica(Stato eventType, PuntoInteresse puntoInteresse) {
        for (Observer listener : osservatori) {
            listener.riceviNotifica(eventType, puntoInteresse);
        }
    }


    public ArrayList<Observer> getOsservatori() {
        return osservatori;
    }

    public void notifica(Stato eventType, MaterialeGenerico materialeGenerico) {
        for (Observer listener : osservatori) {
            listener.riceviNotifica(eventType, materialeGenerico);
        }
    }


    @Override
    public String toString() {
        return "Curatore{" +
                "comune=" + getComune() +
                ", nome=" + getNome() + ", id=" + getId() +
                '}';
    }
}
