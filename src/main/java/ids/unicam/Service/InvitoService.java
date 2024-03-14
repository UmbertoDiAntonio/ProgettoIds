package ids.unicam.Service;

import ids.unicam.exception.ContestException;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.TuristaAutenticato;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Optional;

public interface InvitoService {
    void deleteById(int id);

    @NotNull Invito save(@NotNull Invito invito);

    /**
     * Ottieni l'invito con l'id cercato, se esiste
     *
     * @param id l'id cercato
     * @return l'invito con l'id indicato, Optional.Empty altrimenti
     */
    @NotNull Optional<Invito> findById(int id);

    /**
     * Ottieni tutti gli inviti della piattaforma
     *
     * @return la lista contenente tutti gli inviti della piattaforma
     */
    @NotNull List<Invito> findAll();

    /**
     * Accetta l'invito a partecipare a un contest
     *
     * @param turistaAutenticato il turista che sta accettando l'invito
     * @param invito             l'invito che si vuole accettare
     * @throws ContestException se non è possibile accettare l'invito
     */
    void accettaInvito(@NotNull TuristaAutenticato turistaAutenticato, @NotNull Invito invito) throws ContestException;

    /**
     * Controlla se un invito è valido
     *
     * @param invito l'invito di cui si vuole controllare la validità
     * @return true se l'invito è valido, false altrimenti
     */
    boolean isValid(@NotNull Invito invito);

    /**
     * Ottieni la lista degli inviti ricevuti dall'utente
     *
     * @param usernameTurista l'utente di cui si stanno cercando gli inviti
     * @return la lista degli inviti trovati
     */
    @NotNull List<Invito> getInvitiRicevuti(@NotNull String usernameTurista);
}
