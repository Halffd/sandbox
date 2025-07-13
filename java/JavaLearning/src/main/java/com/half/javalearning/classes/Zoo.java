package com.half.javalearning.classes;

// Classe Abstrata
abstract class Animal {
    protected String nome;
    protected String tipo;
    protected String especie;
    protected String habitat;
    protected String alimentacao;
    protected int idade;

    public Animal(String nome) {
        this.nome = nome;
    }

    // metodo abstrato
    public abstract void emitirSom();

    // metodo concreto
    public void dormir() {
        System.out.println(nome + ": Zzzz... *sonhos com " + sonhoTipico() + "*");
    }
    public void getDetalhes() {
        System.out.println("Nome: " + nome);
        System.out.println("Tipo: " + tipo);
        System.out.println("Espécie: " + especie);
        System.out.println("Habitat: " + habitat);
        System.out.println("Alimentação: " + alimentacao);
        System.out.println("Idade: " + idade);
    }

    // Cada animal sonha com algo diferente
    protected abstract String sonhoTipico();

    public String getNome() {
        return nome;
    }
}

// Subclasse concreta Cachorro
class Cachorro extends Animal {
    public Cachorro(String nome, int idade) {
        super(nome);
        tipo = "Mamifero";
        especie = "Canis lupus";
        habitat = "Cidade";
        alimentacao = "Carne";
    }

    @Override
    public void emitirSom() {
        System.out.println(nome + ": AU AU AUUUUU! *late freneticamente pulando*");
    }

    @Override
    protected String sonhoTipico() {
        return "ossos enterrados";
    }

    public void abanarRabo() {
        System.out.println(nome + ": *abana o rabo tão rápido que quase decola*");
    }
}

// Subclasse concreta Gato
class Gato extends Animal {
    public Gato(String nome, int idade) {
        super(nome);
        tipo = "Mamifero";
        especie = "Felis catus";
        habitat = "Cidade";
        alimentacao = "Carne";
    }

    @Override
    public void emitirSom() {
        System.out.println(nome + ": MIAAAU MIAU! *olhar de julgamento*");
    }

    @Override
    protected String sonhoTipico() {
        return "dominação mundial";
    }

    public void derrubarObjetos() {
        System.out.println(nome + ": *derruba metodicamente todos os objetos da mesa* MIAU!");
    }
}

// Subclasse concreta Leao
class Leao extends Animal {
    public Leao(String nome, int idade) {
        super(nome);
        tipo = "Mamifero";
        especie = "Puma concolor";
        habitat = "Savana";
        alimentacao = "Carne";
    }

    @Override
    public void emitirSom() {
        System.out.println(nome + ": ROOOAAAR! GRRRROAAAAR! *juba balançando*");
    }

    @Override
    protected String sonhoTipico() {
        return "ser rei da selva";
    }

    public void tipoDeAnimal() {
        System.out.println(nome + ": É um animal selvagem *olhar intimidador*");
    }
}

// Subclasse concreta Elefante
class Elefante extends Animal {
    public Elefante(String nome, int idade) {
        super(nome);
        tipo = "Mamifero";
        especie = "Elephas maximus";
        habitat = "Selva";
        alimentacao = "Planta";
    }

    @Override
    public void emitirSom() {
        System.out.println(nome + ": PRRRRRUUUUUUUUUU! *barulho de tromba e terremoto leve*");
    }

    @Override
    protected String sonhoTipico() {
        return "amendoins gigantes";
    }

    public void jogarAgua() {
        System.out.println(nome + ": *enche a tromba e SPLASH! Todo mundo molhado*");
    }
}

// Subclasse concreta Macaco
class Macaco extends Animal {
    public Macaco(String nome, int idade) {
        super(nome);
        tipo = "Mamifero";
        especie = "Macaca mulatta";
        habitat = "Selva";
        alimentacao = "Carne";
        idade = idade;
    }

    @Override
    public void emitirSom() {
        System.out.println(nome + ": UH UH AH AH! *batendo no peito e pulando nas árvores*");
    }

    @Override
    protected String sonhoTipico() {
        return "bananas infinitas";
    }

    public void roubarChapeu() {
        System.out.println(nome + ": *rouba chapéu de visitante e ri histericamente*");
    }
}

// Subclasse concreta Pinguim
class Pinguim extends Animal {
    public Pinguim(String nome, int idade) {
        super(nome);
        tipo = "Mamifero";
        especie = "Pinguinus impennis";
        habitat = "Geleira";
        alimentacao = "Peixes";
        idade = idade;
    }

    @Override
    public void emitirSom() {
        System.out.println(nome + ": WANK WANK! *barulho de pinguim enquanto desliza na barriga*");
    }

    @Override
    protected String sonhoTipico() {
        return "peixes congelados";
    }

    public void escorregar() {
        System.out.println(nome + ": *desliza na barriga como um profissional do gelo* WEEEEEE!");
    }
}

// Subclasse concreta Girafa
class Girafa extends Animal {
    public Girafa(String nome, int idade) {
        super(nome);
        tipo = "Mamifero";
        especie = "Giraffa camelopardalis";
        habitat = "Selva";
        alimentacao = "Planta";
        idade = idade;
    }

    @Override
    public void emitirSom() {
        System.out.println(nome + ": *sons estranhos de girafa* MMMmmmmRRRfffff *pescoço esticando*");
    }

    @Override
    protected String sonhoTipico() {
        return "folhas no topo das árvores";
    }

    public void comerArvore() {
        System.out.println(nome + ": *se estica 5 metros e come folhas do topo* Nhom nhom!");
    }
}

// Subclasse concreta Hipopotamo
class Hipopotamo extends Animal {
    public Hipopotamo(String nome, int idade) {
        super(nome);
        tipo = "Mamifero";
        especie = "Hippopotamus amphibius";
        habitat = "Selva";
        alimentacao = "Planta";
        idade = idade;
    }

    @Override
    public void emitirSom() {
        System.out.println(nome + ": HUUUUUNGH! *abre bocão gigante e bufa*");
    }

    @Override
    protected String sonhoTipico() {
        return "piscinas de lama";
    }

    public void esmagar() {
        System.out.println(nome + ": *pisa tão forte que o chão treme* THUD THUD THUD!");
    }
}

// Subclasse concreta Flamingo
class Flamingo extends Animal {
    public Flamingo(String nome, int idade) {
        super(nome);
        tipo = "Passaro";
        especie = "Phoenicopterus ruber";
        habitat = "Selva";
        alimentacao = "Carne";
        idade = idade;
    }

    @Override
    public void emitirSom() {
        System.out.println(nome + ": HONK HONK! *grasnido rosa*");
    }

    @Override
    protected String sonhoTipico() {
        return "lagoas cor-de-rosa";
    }

    public void ficarNumaPerna() {
        System.out.println(nome + ": *equilibra perfeitamente numa perna por horas* #FlexãoDeGarça");
    }
}

/**
 * Classe Zoo que representa um zoológico ultra-grande.
 * Esta classe contém um array de animais e um número de caos.
 */
public class Zoo {
    private Animal[] animais;
    private String nome;
    private boolean aberto;
    private int nivelCaos;

    public Zoo(String nome) {
        this.nome = nome;
        this.aberto = true;
        this.nivelCaos = 0;

        // Inicializando nosso exército animal
        this.animais = new Animal[] {
                new Cachorro("Rex", 5),
                new Gato("Whiskers", 3),
                new Leao("Simba", 2),
                new Elefante("Dumbo", 1),
                new Macaco("George", 4),
                new Pinguim("Pingu", 6),
                new Girafa("Alto", 7),
                new Hipopotamo("Moto-Moto", 8),
                new Flamingo("Pinky", 9),
                new Cachorro("Max", 10),
                new Gato("Garfield", 11),
                new Leao("Mufasa", 12)
        };
    }

    public void startFamilyTrip() {
        System.out.println("\n🎈🎈🎈 PASSEIO FAMILIAR NO " + nome.toUpperCase() + " 🎈🎈🎈");
        System.out.println("*Música temática de parque toca*");
        System.out.println("Guia: Bem-vindos queridas famílias! Mantenham as crianças por perto!");

        // Apresentação suave dos animais
        for (int i = 0; i < animais.length; i++) {
            System.out.println("\n👨‍👩‍👧‍👦 Família se aproxima do habitat " + (i+1));
            System.out.println("Guia: E aqui temos o adorável " + animais[i].getNome() + "!");
            animais[i].emitirSom();
            animais[i].getDetalhes();
            // Comportamentos especiais aleatórios
            if (animais[i] instanceof Cachorro) {
                ((Cachorro)animais[i]).abanarRabo();
                System.out.println("Criança: OLHA MAMÃE QUE FOFO!");
            } else if (animais[i] instanceof Elefante && i % 2 == 0) {
                ((Elefante)animais[i]).jogarAgua();
                System.out.println("*Família grita e ri* AAAHHH ESTAMOS MOLHADOS!!");
            } else if (animais[i] instanceof Macaco && i % 3 == 0) {
                ((Macaco)animais[i]).roubarChapeu();
                System.out.println("Pai: HEY! DEVOLVE MEU CHAPÉU!");
            }

            // Pausa para fotos
            System.out.println("*Barulho de 50 celulares tirando fotos* CLICK CLICK CLICK");

            // Algo inesperado a cada 4 animais
            if (i % 4 == 3) {
                System.out.println("\n⚠️ MOMENTO INESPERADO ⚠️");
                int surpresa = (int)(Math.random() * 3);
                switch(surpresa) {
                    case 0:
                        System.out.println("*Bebê começa a chorar histericamente*");
                        System.out.println("Todos os animais: *olham assustados*");
                        break;
                    case 1:
                        System.out.println("*Sorvete de uma criança cai no chão*");
                        System.out.println("Criança: BUAAAAAAA! MEU SORVETE!");
                        System.out.println("Animais próximos: *começam a fazer barulho em solidariedade*");
                        break;
                    case 2:
                        System.out.println("*Homem tropeça e cai de cara no mapa do zoo*");
                        System.out.println("Todos: HAHAHAHAHAHA");
                        System.out.println("Animais: *parecem estar rindo também*");
                        break;
                }
            }
        }

        // Final feliz do passeio
        System.out.println("\n🎉 FIM DO PASSEIO FAMILIAR 🎉");
        System.out.println("Guia: Esperamos que tenham se divertido! Passem na lojinha de lembrancinhas!");
        System.out.println("*Crianças implorando por brinquedos de pelúcia*");
        System.out.println("*Pais olhando para a carteira vazia e chorando*");
    }

    public void startSchoolTrip() {
        System.out.println("\n🚌🚌🚌 EXCURSÃO ESCOLAR NO " + nome.toUpperCase() + " 🚌🚌🚌");
        System.out.println("*Som de 50 crianças gritando ao mesmo tempo*");
        System.out.println("Professor: SILÊNCIO CRIANÇAS! Formem filas de dois!");
        System.out.println("*Ninguém forma fila alguma*");

        // Caos controlado com objetivo educacional
        nivelCaos = 10; // Numa escala de 1 a 10

        System.out.println("Guia do Zoo: Olá criançada! Hoje vamos aprender sobre os animais!");
        System.out.println("Alunos: EEEEEEEEHHHHHHH!!!");

        // Visita educativa caótica
        for (int i = 0; i < animais.length; i++) {
            System.out.println("\n📚 PARADA EDUCATIVA " + (i+1) + " 📚");
            System.out.println("Guia: Este é " + animais[i].getNome() + "! Alguém sabe que animal é este?");

            // Respostas aleatórias dos alunos
            String[] respostasDoidas = {
                    "É UM DINOSSAURO!",
                    "Meu pai ronca igual!",
                    "Posso ir ao banheiro?",
                    "Ele come gente?",
                    "Eu tenho um desses em casa!",
                    "Tia, que horas é o lanche?",
                    "Posso fazer carinho?",
                    "Ele tem Instagram?"
            };

            int numRespostas = 2 + (int)(Math.random() * 3); // 2 a 4 respostas
            for (int j = 0; j < numRespostas; j++) {
                int idxResposta = (int)(Math.random() * respostasDoidas.length);
                System.out.println("Aluno random: " + respostasDoidas[idxResposta]);
            }

            // Reação do animal
            animais[i].emitirSom();
            animais[i].getDetalhes();

            // Fato educacional
            System.out.println("Guia: Sabia que...");
            switch (animais[i].getClass().getSimpleName()) {
                case "Cachorro":
                    System.out.println("O olfato de um cachorro é 40 vezes mais poderoso que o humano!");
                    break;
                case "Gato":
                    System.out.println("Gatos passam 70% da vida dormindo!");
                    break;
                case "Leao":
                    System.out.println("O rugido de um leão pode ser ouvido a até 8km de distância!");
                    break;
                case "Elefante":
                    System.out.println("Elefantes são os únicos mamíferos que não conseguem pular!");
                    break;
                case "Macaco":
                    System.out.println("Macacos também têm impressões digitais únicas como humanos!");
                    break;
                case "Pinguim":
                    System.out.println("Pinguins podem beber água salgada porque têm uma glândula que filtra o sal!");
                    break;
                case "Girafa":
                    System.out.println("A língua de uma girafa pode ter até 45cm!");
                    break;
                case "Hipopotamo":
                    System.out.println("Hipopótamos podem correr mais rápido que humanos, até 30km/h!");
                    break;
                case "Flamingo":
                    System.out.println("Flamingos são rosa porque comem camarões com pigmentos rosados!");
                    break;
            }

            // Interação caótica com os animais
            if (nivelCaos > 5) {
                System.out.println("\n🌪️ MOMENTO DE CAOS ESCOLAR 🌪️");
                int tipoDeConfusao = (int)(Math.random() * 5);
                switch(tipoDeConfusao) {
                    case 0:
                        System.out.println("*Duas crianças começam a imitar o animal freneticamente*");
                        System.out.println("Professor: JÁ CHEGA, PEDRO E MARIA!");
                        animais[i].emitirSom(); // Animal responde
                        break;
                    case 1:
                        System.out.println("*Criança joga lanche para o animal*");
                        System.out.println("Guia: NÃO ALIMENTE OS ANIMAIS!!!");
                        System.out.println("*Seguranças do zoo olham com cara feia*");
                        break;
                    case 2:
                        System.out.println("*Três alunos saem correndo e o professor persegue*");
                        System.out.println("Professor: VOLTEM AQUI AGORA!!!");
                        System.out.println("*" + animais[i].getNome() + " observa a cena confuso*");
                        break;
                    case 3:
                        System.out.println("Criança esperta: Na verdade, estudos recentes mostram que...");
                        System.out.println("Professor: *suspiro* Sim Carlos, você já leu a Wikipedia inteira...");
                        break;
                    case 4:
                        System.out.println("*Alguém grita* SELFIE COM O " + animais[i].getNome().toUpperCase() + "!!!");
                        System.out.println("*30 crianças se amontoam na grade fazendo biquinho*");
                        System.out.println("Guia: *facepalm* Todo dia isso...");
                        break;
                }
            }

            // Momento educacional que ninguém presta atenção
            System.out.println("\nProfessor: Agora, façam anotações sobre este animal para o relatório!");
            System.out.println("*Som de lápis rabiscando qualquer coisa menos informações úteis*");
        }

        // Fechamento da excursão
        System.out.println("\n📋 FIM DA EXCURSÃO ESCOLAR 📋");
        System.out.println("Professor: CONTAGEM DE ALUNOS! 1, 2, 3...");
        System.out.println("*Professor perde a conta três vezes seguidas*");
        System.out.println("Professor: ...48, 49... ESPERA! CADÊ O JOÃO?");
        System.out.println("*João aparece comendo algodão doce*");
        System.out.println("Professor: *suspiro profundo* Pelo menos voltamos com o mesmo número de crianças...");
        System.out.println("Guia: *sussurrando* Mais um dia, mais um milagre...");
    }

    public static void main(String[] args) {
        System.out.println("🦁🐘🐒 BEM-VINDO AO ULTRA MEGA ZOOLÓGICO JAVA 2025 🦊🦒🦓");
        System.out.println("*Efeitos sonoros de selva misturados com música épica*");

        Zoo zoo = new Zoo("ANIMAL-LANDIA EXTREME");

        // Um dia no zoológico...
        System.out.println("\n===== MANHÃ: PASSEIO FAMILIAR =====");
        zoo.startFamilyTrip();

        System.out.println("\n\n===== TARDE: EXCURSÃO ESCOLAR =====");
        zoo.startSchoolTrip();

        // Grande finale
        System.out.println("\n\n🎭 GRANDE FINALE - DESFILE NOTURNO DE ANIMAIS 🎭");
        System.out.println("*Luzes coloridas, música dramática, fumaça de gelo seco*");

        // Desfile final de todos os animais
        for (Animal animal : zoo.animais) {
            System.out.println("\n🌟 Apresentando: " + animal.getNome() + " 🌟");
            animal.emitirSom();

            // Comportamentos especiais de cada tipo
            if (animal instanceof Cachorro) ((Cachorro)animal).abanarRabo();
            else if (animal instanceof Gato) ((Gato)animal).derrubarObjetos();
            else if (animal instanceof Leao) ((Leao)animal).tipoDeAnimal();
            else if (animal instanceof Elefante) ((Elefante)animal).jogarAgua();
            else if (animal instanceof Macaco) ((Macaco)animal).roubarChapeu();
            else if (animal instanceof Pinguim) ((Pinguim)animal).escorregar();
            else if (animal instanceof Girafa) ((Girafa)animal).comerArvore();
            else if (animal instanceof Hipopotamo) ((Hipopotamo)animal).esmagar();
            else if (animal instanceof Flamingo) ((Flamingo)animal).ficarNumaPerna();

            System.out.println("*Público aplaude freneticamente*");
        }

        // Encerramento épico
        System.out.println("\n🎆 FIM DO ESPETÁCULO 🎆");
        System.out.println("*Fogos de artifício digitais*");
        System.out.println("*Os animais fazem reverência*");
        System.out.println("*O sistema de som toca 'Circle of Life' do Rei Leão*");
        System.out.println("\nObrigado por visitar o ULTRA MEGA ZOOLÓGICO JAVA 2025!");
        System.out.println("Volte amanhã para mais caos animal controlado por polimorfismo!");
    }
}