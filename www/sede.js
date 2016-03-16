$(function(){
  $('.js-range-slider').prop('disabled',true);
  $('#mod').change(function(e){
    $('#sliderContainer.js-range-slider').prop('disabled',$(this).prop('cheked'));
  });
})