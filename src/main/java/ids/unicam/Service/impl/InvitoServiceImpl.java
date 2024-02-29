package ids.unicam.Service.impl;

import ids.unicam.DataBase.Repository.InvitoRepository;
import ids.unicam.Service.InvitoService;
import ids.unicam.models.Invito;
import ids.unicam.models.attori.TuristaAutenticato;
import jakarta.transaction.Transactional;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

import static ids.unicam.Main.logger;

@Service
public class InvitoServiceImpl implements InvitoService {
    private final InvitoRepository repository;
    private final ContestServiceImpl contestServiceImpl;

    @Autowired
    public InvitoServiceImpl(InvitoRepository repository, ContestServiceImpl contestServiceImpl) {
        this.repository = repository;
        this.contestServiceImpl = contestServiceImpl;
    }


    public void deleteById(int id) {
        repository.deleteById(id);
    }


    public Invito save(Invito invito) {
        return repository.save(invito);
    }


    public Optional<Invito> findById(int id) {
        return repository.findById(id);
    }


    public List<Invito> findAll() {
        return repository.findAll();
    }


    public void deleteAll() {
        repository.deleteAll();
    }

    @Transactional
    @Override
    public void accettaInvito(TuristaAutenticato turistaAutenticato, Invito invito) throws IllegalArgumentException {
        if (isValid(invito)) {
            if (invito.getInvitato().getUsername().equals(turistaAutenticato.getUsername())) {
                contestServiceImpl.aggiungiPartecipante(invito.getContest(), turistaAutenticato);
            } else {
                logger.error("Non sei Invitato");
                throw new IllegalArgumentException("Non sei Invitato");
            }
        } else {
            logger.warn("Invito non valido");
            throw new IllegalArgumentException("Invito non valido");
        }
    }

    @Override
    public boolean isValid(Invito invito) {
        return !invito.getContest().isOpen()
                && !contestServiceImpl.getPartecipanti(invito.getContest()).contains(invito.getInvitato())
                && invito.isValido();
    }

    @Override
    public List<Invito> getInvitiRicevuti(String usernameTurista) {
        return repository.findInvitiByTurista(usernameTurista);
    }
}
