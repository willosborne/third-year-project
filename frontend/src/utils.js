function preloadImages(urls, callback) {
  var images = [];
  var loadedImages = 0;
  var imgCount = urls.length;

  if (imgCount == 0)
    callback([]);
  
  for (var i = 0; i < urls.length; i++) {
    images.push(new Image());
    images[i].onload = function() {
      if (++loadedImages >= imgCount) {
        callback(images);
      }
    }
    // TODO: gracefully recover from GET errors on missing images
    // images[i].onerror = function () {
      
    // }
    images[i].src = urls[i];
  }
}

function preloadImage(url, callback) {
  var img = new Image();
  img.onload = function() {
    callback(img);
  }
  img.src = url;
}
