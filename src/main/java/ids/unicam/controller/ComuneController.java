package ids.unicam.controller;

import ids.unicam.Comune;
import ids.unicam.models.Service.ComuneService;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;


@Controller
public class  ComuneController {
    private final ComuneService service;
    @Autowired
    public ComuneController(ComuneService service){
        this.service = service;
    }

    public Comune creaComune(String nome){
        return service.save(new Comune(nome));
    }


    public @Nullable Comune getComune(String nome){//TODO è una chiamata DB
        return service.findByNome(nome);
    }



}
