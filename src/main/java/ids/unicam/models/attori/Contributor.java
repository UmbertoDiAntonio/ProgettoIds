package ids.unicam.models.attori;

import ids.unicam.models.Comune;
import jakarta.persistence.*;

import java.util.GregorianCalendar;

@Entity
@DiscriminatorValue("Contributor")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
public class Contributor extends TuristaAutenticato  {
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
    public String toString() {
        return "Contributor " +getUsername()+" "+
                "(" + comune +")"+
                '}';
    }


}
