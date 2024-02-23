package ids.unicam.Service;

import ids.unicam.exception.ConnessioneFallitaException;
import ids.unicam.models.DTO.RichiestaCreazioneContributorDTO;
import ids.unicam.models.DTO.TuristaAutenticatoDTO;
import ids.unicam.models.attori.Ruolo;
import ids.unicam.models.attori.TuristaAutenticato;
import org.jetbrains.annotations.NotNull;

public interface GestorePiattaformaService {
    TuristaAutenticato cambiaRuolo(String usernameContributor, @NotNull Ruolo ruolo) throws IllegalArgumentException, ConnessioneFallitaException;

    TuristaAutenticato registraTurista(TuristaAutenticatoDTO turistaDTO) throws IllegalArgumentException;

    TuristaAutenticato registraContributor(RichiestaCreazioneContributorDTO contributorDTO) throws IllegalArgumentException, ConnessioneFallitaException;

}
