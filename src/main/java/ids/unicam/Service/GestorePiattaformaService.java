package ids.unicam.Service;

import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.ContributorDTO;
import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.attori.Ruolo;
import ids.unicam.models.attori.TuristaAutenticato;
import ids.unicam.models.contenuti.RuoloRegistrazione;
import org.jetbrains.annotations.NotNull;

public interface GestorePiattaformaService {
    TuristaAutenticato cambiaRuolo(String usernameContributor, @NotNull Ruolo ruolo) throws IllegalArgumentException, ConnessioneFallitaException;


    TuristaAutenticato registra(ContributorDTO contributorDTO, RuoloRegistrazione ruolo) throws IllegalArgumentException, ConnessioneFallitaException;

}
