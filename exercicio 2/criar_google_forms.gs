function criarFormularioLista2() {
  var form = FormApp.create('Participação eleitoral, governo e representação política');

  form.setDescription(
    'Você está sendo convidado(a) a responder uma pesquisa curta sobre política, governo e representação no Brasil. ' +
    'O questionário é anônimo, autoaplicado e leva cerca de 3 a 4 minutos. Não existem respostas certas ou erradas. ' +
    'Leia cada pergunta com calma e responda de acordo com sua opinião. A participação é voluntária, e você pode encerrar o formulário a qualquer momento.'
  );

  form.setCollectEmail(false);
  form.setAllowResponseEdits(false);
  form.setShowLinkToRespondAgain(false);

  var consentimento = form.addMultipleChoiceItem()
    .setTitle('Você concorda em participar desta pesquisa anônima?')
    .setRequired(true);

  var perfil = form.addPageBreakItem().setTitle('Perfil mínimo do respondente');

  consentimento.setChoices([
    consentimento.createChoice('Sim, concordo em participar', perfil),
    consentimento.createChoice('Não concordo em participar', FormApp.PageNavigationType.SUBMIT)
  ]);

  form.addMultipleChoiceItem()
    .setTitle('Qual é a sua faixa de idade?')
    .setRequired(true)
    .setChoiceValues([
      '16 ou 17 anos',
      '18 a 29 anos',
      '30 a 44 anos',
      '45 a 59 anos',
      '60 anos ou mais',
      'Prefiro não responder'
    ]);

  form.addMultipleChoiceItem()
    .setTitle('Em qual região do Brasil você mora atualmente?')
    .setRequired(true)
    .setChoiceValues([
      'Norte',
      'Nordeste',
      'Centro-Oeste',
      'Sudeste',
      'Sul',
      'Moro fora do Brasil',
      'Prefiro não responder'
    ]);

  form.addPageBreakItem().setTitle('Interesse, informação e agenda pública');

  form.addScaleItem()
    .setTitle('Em geral, qual é o seu nível de interesse por política?')
    .setRequired(true)
    .setBounds(1, 5)
    .setLabels('Nenhum interesse', 'Muito interesse');

  form.addMultipleChoiceItem()
    .setTitle('Pensando nos últimos 7 dias, por qual meio você mais se informou sobre política?')
    .setRequired(true)
    .setChoiceValues([
      'Televisão',
      'Rádio',
      'Jornais, sites ou aplicativos de notícia',
      'Redes sociais',
      'Conversas com familiares, amigos ou colegas',
      'Não me informei sobre política nesse período',
      'Outro'
    ]);

  form.addMultipleChoiceItem()
    .setTitle('Na sua opinião, qual destes é hoje o principal problema do Brasil?')
    .setRequired(true)
    .setChoiceValues([
      'Economia e custo de vida',
      'Saúde',
      'Educação',
      'Segurança pública e violência',
      'Corrupção',
      'Meio ambiente',
      'Outro',
      'Não sei avaliar'
    ]);

  form.addPageBreakItem().setTitle('Governo e representação');

  form.addMultipleChoiceItem()
    .setTitle('Como você avalia o governo federal atual?')
    .setRequired(true)
    .setChoiceValues([
      'Ótimo',
      'Bom',
      'Regular',
      'Ruim',
      'Péssimo',
      'Não sei avaliar'
    ]);

  form.addMultipleChoiceItem()
    .setTitle('Na sua opinião, os partidos políticos são importantes para a democracia?')
    .setRequired(true)
    .setChoiceValues([
      'Muito importantes',
      'Um pouco importantes',
      'Pouco importantes',
      'Nada importantes',
      'Não sei avaliar'
    ]);

  form.addScaleItem()
    .setTitle('De modo geral, você acha que os políticos brasileiros escutam as demandas da população?')
    .setRequired(true)
    .setBounds(1, 5)
    .setLabels('Nunca escutam', 'Sempre escutam');

  var preferencia = form.addMultipleChoiceItem()
    .setTitle('Você tem algum partido político de preferência?')
    .setRequired(true);

  var partido = form.addPageBreakItem().setTitle('Preferência partidária');

  form.addTextItem()
    .setTitle('Qual partido político você prefere?')
    .setHelpText('Escreva a sigla ou o nome do partido.')
    .setRequired(true);

  var participacao = form.addPageBreakItem().setTitle('Participação eleitoral');

  preferencia.setChoices([
    preferencia.createChoice('Sim', partido),
    preferencia.createChoice('Não', participacao),
    preferencia.createChoice('Não sei', participacao),
    preferencia.createChoice('Prefiro não responder', participacao)
  ]);

  form.addMultipleChoiceItem()
    .setTitle('Se o voto não fosse obrigatório, qual seria a chance de você votar na próxima eleição para presidente?')
    .setRequired(true)
    .setChoiceValues([
      'Com certeza votaria',
      'Provavelmente votaria',
      'Provavelmente não votaria',
      'Com certeza não votaria',
      'Ainda não sei'
    ]);

  Logger.log('Link para editar: ' + form.getEditUrl());
  Logger.log('Link para responder: ' + form.getPublishedUrl());
}
