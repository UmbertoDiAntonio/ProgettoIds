package ids.unicam.Service;

import ids.unicam.models.Invito;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.Contest;
import ids.unicam.models.contenuti.puntiInteresse.PuntoInteresse;

import java.util.List;
import java.util.Optional;

public interface TuristaAutenticatoService {



    public TuristaAutenticato save(TuristaAutenticato turistaAutenticato) ;

    public void accettaInvitoContest(TuristaAutenticato turistaAutenticato, Invito invito) ;

    public void rimuoviPreferito(TuristaAutenticato turistaAutenticato, int id);

    public void aggiungiPreferito(TuristaAutenticato turista, PuntoInteresse puntoInteresse) ;

    public List<TuristaAutenticato> findTuristiConPreferiti();

    public List<PuntoInteresse> findPreferiti(TuristaAutenticato turistaAutenticato);

    /**
     * Entra nel contest se è aperto
     *
     * @param contest il contest in cui si vuole entrare
     */

    public void partecipaAlContest(Contest contest, TuristaAutenticato turistaAutenticato) ;


    public Optional<TuristaAutenticato> findTuristaByUsername(String username) ;

    public boolean verificaPassword(String password, String username) ;

    public boolean isUsernameUnique(String username);
}
