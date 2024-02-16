package ids.unicam.models.attori;

import ids.unicam.models.Comune;
import ids.unicam.models.contenuti.MaterialeGenerico;
import ids.unicam.models.contenuti.PuntoInteresse;
import ids.unicam.models.Observer;
import ids.unicam.models.Stato;
import jakarta.persistence.*;

import java.util.GregorianCalendar;

@Entity
@DiscriminatorValue("Contributor")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class Contributor extends TuristaAutenticato implements Observer  {
    @OneToOne
    @JoinColumn(name = "comune")
    private Comune comune = null;

    public Contributor() {

    }


    public Comune getComune() {
        return comune;
    }


    public Contributor(Comune comune, TuristaAutenticato turistaAutenticato) {
        super(turistaAutenticato.getNome(), turistaAutenticato.getCognome(), turistaAutenticato.getDataNascita(), turistaAutenticato.getPassword(), turistaAutenticato.getUsername());
        this.comune = comune;
    }

    public Contributor(Comune comune, String name, String surname, GregorianCalendar dateBirthday, String password, String username) {
        super(name, surname, dateBirthday, password, username);
        this.comune = comune;
    }

    @Override
    public void riceviNotifica(Stato eventType, PuntoInteresse puntoInteresse) {
        switch (eventType) {
            case APPROVED ->
                    System.out.println("Il tuo " + puntoInteresse.mostraInformazioniGeneriche() + " e' stato approvato");
            case NOT_APPROVED ->
                    System.out.println("Il tuo " + puntoInteresse.mostraInformazioniGeneriche() + " non e' stato approvato");
        }
    }

    @Override
    public void riceviNotifica(Stato eventType, MaterialeGenerico materialeGenerico) {
        switch (eventType) {
            case APPROVED ->
                    System.out.println("Il tuo contenuto relativo al punto di interesse " + materialeGenerico.get() + " e' stato approvato");
            case NOT_APPROVED ->
                    System.out.println("Il tuo contenuto relativo al punto di interesse " + materialeGenerico.get() + " non e' stato approvato");
        }
    }




    @Override
    public String toString() {
        return "Contributor{" +
                "comune=" + comune +
                ", nome=" +getNome()+", id="+getId()+
                '}';
    }


}
