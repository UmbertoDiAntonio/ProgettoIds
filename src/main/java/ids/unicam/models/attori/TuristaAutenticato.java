package ids.unicam.models.attori;

import ids.unicam.models.contenuti.PuntoInteresse;
import jakarta.persistence.*;

import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.Objects;

@Entity
@Table(name = "TURISTI")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
@DiscriminatorColumn(name = "TIPO")
@DiscriminatorValue("TuristaAutenticato")
public class TuristaAutenticato extends Turista{
    @Id
    @GeneratedValue(strategy = GenerationType.SEQUENCE,generator = "sequenza_utenti")
    @SequenceGenerator(name = "sequenza_utenti", sequenceName = "users", allocationSize = 1)
    private Integer id;
    private String nome ="";
    private String username="";
    private String cognome ="";
    private GregorianCalendar dataNascita = new GregorianCalendar();
    private String password="";
    @OneToMany(fetch = FetchType.EAGER)
    private final List<PuntoInteresse> preferiti = new ArrayList<>();

    public TuristaAutenticato() {

    }

    public String getNome() {
        return nome;
    }

    public String getCognome() {
        return cognome;
    }

    public GregorianCalendar getDataNascita() {
        return dataNascita;
    }

    public String getPassword() {
        return password;
    }

    public String getUsername() {
        return username;
    }

    public Integer getId() {
        return id;
    }

    void setId(Integer id) {
        this.id = id;
    }

    public List<PuntoInteresse> getPreferiti() {
        return preferiti;
    }

    public TuristaAutenticato(String nome, String cognome, GregorianCalendar dataNascita, String password, String username) {
        this.nome = nome;
        this.cognome = cognome;
        this.dataNascita = dataNascita;
        this.password = password;
        this.username = username;
        save();
    }

    private void save() {
        //TODO
    }


    @Override
    public String toString() {
        return "TuristaAutenticato{" +
                "id=" + id +
                ", nome='" + nome + '\'' +
                ", username='" + username + '\'' +
                ", cognome='" + cognome + '\'' +
                ", dataNascita=" + dataNascita +
                ", password='" + password + '\'' +
                ", preferiti=" + preferiti +
                '}';
    }

    public boolean logOut() {
        //TODO
        return true;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        TuristaAutenticato that = (TuristaAutenticato) o;

        if (!id.equals(that.id)) return false;
        if (!Objects.equals(nome, that.nome)) return false;
        if (!Objects.equals(username, that.username)) return false;
        if (!Objects.equals(cognome, that.cognome)) return false;
        if (!Objects.equals(dataNascita, that.dataNascita)) return false;
        return Objects.equals(password, that.password);
    }

    @Override
    public int hashCode() {
        int result = id.hashCode();
        result = 31 * result + (nome != null ? nome.hashCode() : 0);
        result = 31 * result + (username != null ? username.hashCode() : 0);
        result = 31 * result + (cognome != null ? cognome.hashCode() : 0);
        result = 31 * result + (dataNascita != null ? dataNascita.hashCode() : 0);
        result = 31 * result + (password != null ? password.hashCode() : 0);
        return result;
    }
}
