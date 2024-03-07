package ids.unicam.models.attori;

import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;
import jakarta.persistence.*;
import lombok.Getter;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Classe Turista Autenticato, il Turista Autenticato è un utente che non contribuisce in nessun comune,
 * possono partecipare a contest, salvare punti di interesse tra i preferiti, caricare materiali su punti di interesse
 * (i materiali caricati sono posti in stato "IN_ATTESA").
 */
@Getter
@Entity
@Table(name = "TURISTI")
@Inheritance(strategy = InheritanceType.TABLE_PER_CLASS)
@DiscriminatorColumn(name = "TIPO")
@DiscriminatorValue("TuristaAutenticato")
public class TuristaAutenticato {
    @OneToMany(fetch = FetchType.EAGER)
    private final List<PuntoInteresse> preferiti = new ArrayList<>();//TODO unmodifiable List?
    private String nome = "";
    @Id
    private String username = "";
    private String cognome = "";
    private LocalDate dataNascita;
    private String password = "";

    public TuristaAutenticato() {

    }

    public TuristaAutenticato(TuristaAutenticatoDTO turistaDTO) {
        this.nome = turistaDTO.getNome();
        this.cognome = turistaDTO.getCognome();
        this.dataNascita = turistaDTO.getDataNascita();
        this.password = turistaDTO.getPassword();
        this.username = turistaDTO.getUsername();
    }

    @Override
    public String toString() {
        return "TuristaAutenticato{" +
                ", nome='" + nome + '\'' +
                ", username='" + username + '\'' +
                ", cognome='" + cognome + '\'' +
                ", dataNascita=" + dataNascita +
                ", password='" + password + '\'' +
                ", preferiti=" + preferiti +
                '}';
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        TuristaAutenticato that = (TuristaAutenticato) o;
        if (!Objects.equals(nome, that.nome)) return false;
        if (!username.equals(that.username)) return false;
        if (!Objects.equals(cognome, that.cognome)) return false;
        return Objects.equals(password, that.password);
    }

    @Override
    public int hashCode() {
        int result = nome != null ? nome.hashCode() : 0;
        result = 31 * result + username.hashCode();
        result = 31 * result + (cognome != null ? cognome.hashCode() : 0);
        result = 31 * result + (password != null ? password.hashCode() : 0);
        return result;
    }
}
