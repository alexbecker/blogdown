function inlineFootnote(footnote) {
    var ref = document.getElementById("a-" + footnote.id);
    var inline = document.createElement("div");
    for (var i=0; i<footnote.children.length;) {
        var child = footnote.children[i];
        if (child.tagName == "A" && child.getAttribute("href") == "#" + ref.id) {
            i++;
            continue;
        }
        inline.appendChild(child);
    }
    inline.className = "inline-footnote";
    inline.style.display = "none";
    ref.parentNode.parentNode.insertBefore(inline, ref.parentNode.nextSibling);
    ref.origIndex = ref.textContent;
    ref.expanded = false;
    ref.addEventListener("click", function (e) {
        e.preventDefault();
        if (ref.expanded) {
            inline.style.display = "none";
            ref.textContent = ref.origIndex;
        } else {
            inline.style.display = "block";
            ref.textContent = "X";
        }
        ref.expanded = !ref.expanded;
    });
}

function inlineFootnotes() {
    var footnoteLists = document.getElementsByClassName("footnotes");
    while (footnoteLists.length > 0) {
        var footnotes = footnoteLists[0].children;
        for (var j=0; j<footnotes.length; j++) {
            inlineFootnote(footnotes[j]);
        }
        footnoteLists[0].remove();
    }
}

inlineFootnotes()
