document.addEventListener("DOMContentLoaded", function(){
    var log = function(){
        if(console) console.log.apply(null, arguments);
    }
    
    var initUpload = function(root){
        log("Init upload", root);

        var counter = 0;
        var registerNewImage = null;
        
        var addNewImage = function(old){
            log("New image", old);
            // Do this first so subsequent files are not required.
            var file = old.querySelector("[type=file]");
            file.removeAttribute("required");
            // Clone and register a copy of the selector.
            var newImage = old.cloneNode(true);
            newImage.querySelector("[type=file]").value = "";
            root.querySelector(".images").appendChild(newImage);
            registerNewImage(newImage);
            // Turn the old selector into an image preview.
            file.removeAttribute("id");
            old.classList.remove("new-image");
            old.classList.add("image");
            var label = old.querySelector("label");
            label.parentNode.removeChild(label);
            var image = document.createElement("img");
            old.appendChild(image);
            image.addEventListener("dblclick", function(){ old.parentNode.removeChild(old); });
            // Load the image file.
            var reader = new FileReader();
            reader.onload = function(){ image.src = this.result; };
            reader.readAsDataURL(file.files[0]);
        };
        
        registerNewImage = function(old){
            old.setAttribute("data-counter", counter);
            counter++;
            old.querySelector("[type=file]").addEventListener("change", function(){
                addNewImage(old);
            });
        };
        
        registerNewImage(root.querySelector(".new-image"));
    };

    var initGallery = function(root){
        log("Init gallery", root);
    };

    var initView = function(root){
        log("Init view", root);
    };

    var init = function(root){
        var upload = root.querySelector(".upload");
        var gallery = root.querySelector(".gallery");
        var view = root.querySelector(".view");
        
        if(upload) initUpload(upload);
        if(gallery) initGallery(gallery);
        if(view) initView(view);
    };

    init(document);
});
